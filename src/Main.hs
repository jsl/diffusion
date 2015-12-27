{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Monoid ((<>))
import Data.Conduit.Binary  (sinkFile)
import Data.Default (def)
import Network.HTTP.Types.Status (statusCode, Status(..))
import System.Directory (getAppUserDataDirectory)
import Turtle.Prelude
import Turtle.Shell (using, sh, Shell(..), view)
import Control.Applicative (empty)
import Network.HTTP.Types.Header (hLastModified, ResponseHeaders)
import GHC.IO.Exception (ExitCode(ExitSuccess))
import GHC.Exception (SomeException(..))
import Filesystem.Path ((</>))

import qualified Data.Conduit as C
import qualified Network.HTTP.Conduit as HC
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.List as DL
import qualified Filesystem.Path.CurrentOS as FPCOS
import qualified Filesystem.Path as FP
import qualified Options.Applicative as O

type URL = T.Text

data MkOpts = MkOpts { mapRegion    :: String
                     , mapCountry   :: String
                     , cachedBounds :: Bool
                     , cachedSea    :: Bool
                     }

optsParser :: O.Parser MkOpts
optsParser = MkOpts <$> O.strOption
       (  O.long "region"
       <> O.short 'r'
       <> O.metavar "REGION"
       <> O.help "REGION for map"
       ) <*> O.strOption
       (  O.long "country"
       <> O.short 'c'
       <> O.metavar "REGION"
       <> O.help "COUNTRY for map"
       ) <*> O.switch
       (  O.long "cached-bounds"
       <> O.short 'b'
       <> O.help "Don't check for a newer bounds file if a cached version exists"
       ) <*> O.switch
       (  O.long "cached-sea"
       <> O.short 's'
       <> O.help "Don't check for a newer sea file if a cached version exists"
       )

data DownloadJob = DownloadJob
    { jobName    :: T.Text
    , outputName :: FP.FilePath
    , sourceURL  :: URL
    , mvSrc      :: FP.FilePath
    , mvDest     :: FP.FilePath
    , unpackCmd  :: Maybe T.Text
    , checkForUpdate :: Bool
    }

notModified :: Int
notModified = 304

-- | @checkStatus@ implementation that accepts
--   2xx status codes and 304. Calls default implementation
--   on other status codes (i.e. throws exception)
ckstat :: Status -> ResponseHeaders -> HC.CookieJar -> Maybe SomeException
ckstat st@(Status sc _) rh cj =
    if (200 <= sc && sc < 300) || sc == notModified
        then Nothing
        else HC.checkStatus def st rh cj

-- | Retrieves a file only if the remote file was modified after
-- the mtime in the file at the given path (generally under 'stat').
getIfModifiedSince :: FP.FilePath -> FP.FilePath -> URL -> Bool -> Shell Bool
getIfModifiedSince path outputFile url ckForUpdate = do
  manager <- liftIO $ HC.newManager HC.tlsManagerSettings
  req     <- liftIO $ HC.parseUrl $ T.unpack url

  statFileExists <- testfile path

  reqHeaders <- if statFileExists then
                    do
                      t <- input path
                      return [("If-Modified-Since", BS8.pack $ T.unpack t)]
                    else
                      return []

  let req2 = req { HC.requestHeaders = reqHeaders
                 , HC.checkStatus    = ckstat }

  if not statFileExists || ckForUpdate then
      liftIO $ runResourceT $ do
        res <- HC.http req2 manager
        if statusCode (HC.responseStatus res) == notModified then
            do
              echo $ "File '" <> filepathToText (FP.filename outputFile) <>
                       "' already most recent version, using cached file."

              return False

          else
            do
              echo $ "File '" <> filepathToText (FP.filename outputFile) <>
                       "' is outdated or missing, fetching most recent version."

              HC.responseBody res C.$$+- sinkFile $ FPCOS.encodeString outputFile

              case DL.find (\(name, _) -> name == hLastModified) $
                   HC.responseHeaders res of

                Just (_, gmtDate) ->
                    do
                      when statFileExists $ rm path
                      output path (return $ T.pack $ BS8.unpack gmtDate)

                Nothing ->
                    error "Unable to determine Last-Modified time for file, aborting."

              return True

    else
      do
        echo $ "Cached file '" <> filepathToText (FP.filename outputFile) <>
                 "' is present, skipping check for update."

        return False


userEzGmapDirectory :: IO FP.FilePath
userEzGmapDirectory = do
  appDir  <- getAppUserDataDirectory "osm2gmap"
  return $ FPCOS.fromText $ T.pack appDir

statDir :: IO FP.FilePath
statDir = do
  appPath <- userEzGmapDirectory
  return $ appPath </> "stat"

filepathToText :: FP.FilePath -> T.Text
filepathToText fp = case FPCOS.toText fp of
                      Left  _ -> error "Path has invalid encoding!"
                      Right p -> p

-- | Retrieves a file from an Internet source, does necessary post-
-- processing, and installs artifacts to the correct location.
installDependency :: FP.FilePath -> FP.FilePath -> DownloadJob -> Shell ()
installDependency statPath tmpPath dj = do
  let statFilePath = statPath </> FPCOS.fromText (jobName dj <> "-mtime.txt")

  tmpDir <- using (mktempdir tmpPath (jobName dj))

  let tmpFileDest = tmpDir </> outputName dj

  res <- getIfModifiedSince statFilePath tmpFileDest (sourceURL dj) (checkForUpdate dj)

  when res $ do
    cd tmpDir

    case unpackCmd dj of
      Just cmd -> shell cmd empty
      Nothing -> return ExitSuccess

    echo $ "Moving " <> filepathToText (mvSrc dj) <> " to " <>
           filepathToText (mvDest dj)

    isDir <- testdir $ mvDest dj

    when isDir $ rmtree (mvDest dj)
    mv (mvSrc  dj) (mvDest dj)

-- | Describes dependencies to be installed.
downloadJobs :: FP.FilePath -- ^ Path where binaries are installed
             -> FP.FilePath -- ^ Path where data files are installed
             -> T.Text -- ^ Region being mapped
             -> T.Text -- ^ Country (in region) being mapped
             -> MkOpts -- ^ Configuration options
             -> [DownloadJob] -- ^ Returned list of dependencies
downloadJobs binPath dataPath region country cfg =
  [ DownloadJob
      { jobName = "mkgmap"
      , outputName = "mkgmap.zip"
      , mvSrc = "mkgmap"
      , mvDest = binPath </> "mkgmap"
      , sourceURL = "http://www.mkgmap.org.uk/download/mkgmap-latest.zip"
      , unpackCmd = Just "unzip mkgmap.zip && mv mkgmap-r* mkgmap"
      , checkForUpdate = True }

  , DownloadJob
      { jobName = "splitter"
      , outputName = "splitter.zip"
      , mvSrc = "splitter"
      , mvDest = binPath </> "splitter"
      , sourceURL = "http://www.mkgmap.org.uk/download/splitter-latest.zip"
      , unpackCmd = Just "unzip splitter.zip && mv splitter-r* splitter"
      , checkForUpdate = True }

  , DownloadJob
      { jobName = region <> "-" <> country
      , outputName = FPCOS.fromText countryFname
      , mvSrc = FPCOS.fromText countryFname
      , mvDest = dataPath </> FPCOS.fromText countryFname
      , sourceURL = "http://download.geofabrik.de/" <> region <> "/" <>
                    countryFname
      , unpackCmd = Nothing
      , checkForUpdate = True }

  , DownloadJob
      { jobName = "bounds"
      , outputName = "bounds.zip"
      , mvSrc = "bounds.zip"
      , mvDest = dataPath </> "bounds.zip"
      , sourceURL =
        "http://osm2.pleiades.uni-wuppertal.de/bounds/latest/bounds.zip"
      , unpackCmd = Nothing
      , checkForUpdate = not $ cachedBounds cfg }

  , DownloadJob
      { jobName = "sea"
      , outputName = "sea.zip"
      , mvSrc = "sea.zip"
      , mvDest = dataPath </> "sea.zip"
      , sourceURL = "http://osm2.pleiades.uni-wuppertal.de/sea/latest/sea.zip"
      , unpackCmd = Nothing
      , checkForUpdate = not $ cachedSea cfg }

  , DownloadJob
      { jobName = "gmapi-builder"
      , outputName = "gmapi-builder.tar.gz"
      , mvSrc = "gmapi-builder/gmapi-builder.py"
      , mvDest = binPath </> "gmapi-builder.py"
      , sourceURL =
        "http://bitbucket.org/berteun/gmapibuilder/downloads/" <>
        "gmapi-builder.tar.gz"
      , unpackCmd = Just "tar -xvzf gmapi-builder.tar.gz"
      , checkForUpdate = True }
  ]

  where countryFname = country <> "-latest.osm.pbf"

-- | Initializes application directories
initializeDirectories ::
  FP.FilePath -- ^ The main application directory
  -> FP.FilePath -- ^ The directory for status files
  -> Shell (FP.FilePath, FP.FilePath, FP.FilePath, FP.FilePath)
  -- ^ Returned operating directory names
initializeDirectories appPath statPath = do
  isDir <- testdir outputPath
  when isDir $ rmtree outputPath

  mapM_ mktree [statPath, binPath, dataPath, tmpPath, outputPath]

  return (binPath, dataPath, outputPath, tmpPath)

  where binPath      = appPath </> "bin"
        dataPath     = appPath </> "data"
        tmpPath      = appPath </> "tmp"
        outputPath   = appPath </> "output"

opts :: O.ParserInfo MkOpts
opts = O.info (O.helper <*> optsParser)
       ( O.fullDesc
         <> O.progDesc "Makes an OSM map for a Garmin device"
         <> O.header "osm2gmap - Makes an OSM map for a Garmin device"
       )

main :: IO ()
main = do
  cfg <- O.execParser opts

  appPath  <- userEzGmapDirectory
  statPath <- statDir

  sh $ do

    (binPath, dataPath, outputPath, tmpPath) <-
      initializeDirectories appPath statPath

    mapM_ (installDependency statPath tmpPath)
      (downloadJobs binPath dataPath (T.pack $ mapRegion cfg) (T.pack $ mapCountry cfg) cfg)

    echo "Starting to split..."

    cd binPath
    splitOutputPath <- using (mktempdir tmpPath "split-output")

    let splitterCmd = "java -jar splitter/splitter.jar --output-dir=" <>
                      filepathToText splitOutputPath <> " " <>
                      filepathToText dataPath <> T.pack "/" <>
                      T.pack (mapCountry cfg) <>
                      T.pack "-latest.osm.pbf"

    echo $ "splitter command: " <> splitterCmd

    shell splitterCmd empty

    mkgmapOutputPath <- using (mktempdir tmpPath "mkgmap-output")

    let mapName = "OSM " <> T.pack (mapCountry cfg)

    let mkgmapCmd =
          "java -jar mkgmap/mkgmap.jar" <> " --route" <>
          " --add-pois-to-areas" <> " --family-name=\"" <>  mapName <> "\"" <>
          " --series-name=\"" <> mapName <> "\"" <> " --description=\"" <>
          mapName <> "\"" <> " --mapname=55500001" <> " --latin1" <>
          " --precomp-sea=" <> filepathToText dataPath <> "/sea.zip" <>
          " --bounds=" <> filepathToText dataPath <> "/bounds.zip" <>
          " --index" <> " --output-dir=" <> filepathToText mkgmapOutputPath <>
          " --gmapsupp " <> filepathToText splitOutputPath <> "/*.osm.pbf"

    echo "Starting to make map..."

    echo $ "mkgmap command: " <> mkgmapCmd

    shell mkgmapCmd empty

    mv (mkgmapOutputPath </> "gmapsupp.img")
       (outputPath </> "gmapsupp.img")

    -- Now that we've removed the gmapsupp file, we can use the rest of
    -- the images from the previous step to create a map for Garmin
    -- basecamp.

    echo "Generated files from mkgmap:"
    view (ls mkgmapOutputPath)

    let gmapCmd =
          "./gmapi-builder.py -t " <> filepathToText mkgmapOutputPath <>
          "/osmmap.tdb" <> " -o " <> filepathToText outputPath <>
          " -b " <> filepathToText mkgmapOutputPath <> "/osmmap.img " <>
          filepathToText mkgmapOutputPath <> "/*.img"

    echo $ "gmapi-builder command: " <> gmapCmd

    shell gmapCmd empty

    echo $ "Output files copied to " <> filepathToText outputPath

    echo ""
    echo "Installation instructions:"
    echo ""
    echo $ "Your completed maps have been placed in " <>
      filepathToText outputPath <> ":"

    echo $ "Basecamp map (to copy to ~/Library/Application\\ " <>
      "Support/Garmin/Maps/): " <> filepathToText outputPath <>
      "/" <>  mapName <> ".gmapi/" <> mapName <> ".gmap/"

    echo $ "The map for installation in your Garmin device: " <>
      filepathToText outputPath <> "/gmapsupp.img"

    echo ""

    echo "All done!"
