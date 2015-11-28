{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (runResourceT)

import Data.Conduit.Binary  (sinkFile)
import Network.HTTP.Types.Status (statusCode, Status(..))
import System.Directory (getAppUserDataDirectory)
import Data.Default (def)
import Control.Monad (when, liftM)


import Turtle.Prelude
import Turtle.Shell (using, sh, Shell(..), view)
import Turtle.Format (repr)
import Control.Applicative (empty)
import Network.HTTP.Types.Header (hLastModified)
import GHC.IO.Exception (ExitCode(ExitSuccess))
import GHC.Exception (SomeException(..))
import Network.HTTP.Types.Header (ResponseHeaders)
import System.Environment (getEnv)

import qualified Data.Conduit as C
import qualified Network.HTTP.Conduit as HC
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.List as DL
import qualified Filesystem.Path.CurrentOS as FPCOS
import qualified Filesystem.Path as FP

type URL = T.Text

data DownloadJob = DownloadJob
    { jobName    :: T.Text
    , outputName :: FP.FilePath
    , sourceURL  :: URL
    , tmpPath    :: FP.FilePath
    , mvSrc      :: FP.FilePath
    , mvDest     :: FP.FilePath
    , unpackCmd  :: Maybe T.Text
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
getIfModifiedSince :: FP.FilePath -> FP.FilePath -> URL -> Shell Bool
getIfModifiedSince path outputFile url = do
  manager <- liftIO $ HC.newManager HC.tlsManagerSettings
  req     <- liftIO $ HC.parseUrl $ T.unpack url

  statFileExists <- testfile path

  reqHeaders <- if statFileExists then
                    do
                      t <- input path
                      return [("If-Modified-Since", BS8.pack $ T.unpack t)]
                    else do
                      return []

  let req2 = req { HC.requestHeaders = reqHeaders
                 , HC.checkStatus    = ckstat }

  liftIO $ runResourceT $ do
    res <- HC.http req2 manager
    if statusCode (HC.responseStatus res) == notModified then
        do
          echo $ T.concat
                   [ "File '", filepathToText outputFile
                   , "' already most recent version, using cached file." ]

          return False
    else
        do
          echo $ T.concat
                   [ "File '", filepathToText outputFile
                   , "' is outdated or missing, fetching most recent version." ]

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

userEzGmapDirectory :: IO FP.FilePath
userEzGmapDirectory = do
  appDir  <- getAppUserDataDirectory "osm2gmap"
  return $ FPCOS.fromText $ T.pack appDir

statDir :: IO FP.FilePath
statDir = do
  appPath <- userEzGmapDirectory
  return $ FP.append appPath (FPCOS.fromText $ T.pack "stat")

filepathToText :: FP.FilePath -> T.Text
filepathToText fp = case FPCOS.toText fp of
                      Left  _ -> error "Path has invalid encoding!"
                      Right p -> p

-- | Retrieves a file from an Internet source, does necessary post-
-- processing, and installs artifacts to the correct location.
installDependency :: FP.FilePath -> DownloadJob -> Shell ()
installDependency statPath dj = do
  let statFilePath = FP.append statPath $ FPCOS.fromText $
                     T.append (jobName dj) "-mtime.txt"

  tmpDir <- using (mktempdir (tmpPath dj) (jobName dj))

  let tmpFileDest = FP.append tmpDir (outputName dj)

  res <- getIfModifiedSince statFilePath tmpFileDest (sourceURL dj)

  when res $ do
    cd tmpDir

    case unpackCmd dj of
      Just cmd -> shell cmd empty
      Nothing -> return ExitSuccess

    echo $ T.concat [ "Moving ", filepathToText (mvSrc dj), " to "
                    , filepathToText (mvDest dj)]

    isDir <- testdir $ mvDest dj

    when isDir $ rmtree (mvDest dj)
    mv (mvSrc  dj) (mvDest dj)


-- | The release number of mkgmap that will be used.
mkgmapRel :: Int
mkgmapRel = 3652

-- | The release of splitter that will be used.
splitterRel :: Int
splitterRel = 427

main :: IO ()
main = sh $ do

  region  <- liftIO $ liftM T.pack $ getEnv "MAP_REGION"
  country <- liftIO $ liftM T.pack $ getEnv "MAP_COUNTRY"

  appPath  <- liftIO userEzGmapDirectory
  statPath <- liftIO statDir

  let binPath    = FP.append appPath "bin"
      dataPath   = FP.append appPath "data"
      tpath      = FP.append appPath "tmp"
      outputPath = FP.append appPath "output"
      countryFname = T.append country "-latest.osm.pbf"

  isDir <- testdir outputPath
  when isDir $ rmtree outputPath

  mapM_ mktree [statPath, binPath, dataPath, tpath, outputPath]

  mapM_ (installDependency statPath)
    [ DownloadJob { jobName = "mkgmap"
                  , outputName = "mkgmap.zip"
                  , tmpPath = tpath
                  , mvSrc =
                    FPCOS.fromText $ T.concat ["mkgmap-r", repr mkgmapRel]
                  , mvDest = FP.append binPath "mkgmap"
                  , sourceURL = T.concat
                                [ "http://www.mkgmap.org.uk/download/mkgmap-r"
                                , repr mkgmapRel, ".zip" ]
                  , unpackCmd = Just (T.unwords ["unzip mkgmap.zip"]) }

    , DownloadJob { jobName = "splitter"
                  , outputName = "splitter.zip"
                  , tmpPath = tpath
                  , mvSrc = FPCOS.fromText $
                            T.concat ["splitter-r", repr splitterRel]
                  , mvDest = FP.append binPath "splitter"
                  , sourceURL = T.concat
                                [ "http://www.mkgmap.org.uk/download/splitter-r"
                                , repr splitterRel
                                , ".zip" ]
                  , unpackCmd = Just (T.unwords ["unzip splitter.zip"]) }

    , DownloadJob { jobName = "country"
                  , outputName = FPCOS.fromText countryFname
                  , tmpPath = tpath
                  , mvSrc = FPCOS.fromText $ countryFname
                  , mvDest = FP.append dataPath $ FPCOS.fromText $ countryFname
                  , sourceURL = T.concat
                                [ "http://download.geofabrik.de/"
                                , region
                                , "/"
                                , countryFname ]
                  , unpackCmd = Nothing }

    , DownloadJob { jobName = "bounds"
                  , outputName = "bounds.zip"
                  , tmpPath = tpath
                  , mvSrc = "bounds.zip"
                  , mvDest = FP.append dataPath "bounds.zip"
                  , sourceURL = T.concat
                                [ "http://osm2.pleiades.uni-wuppertal.de/"
                                , "bounds/latest/bounds.zip" ]
                  , unpackCmd = Nothing }

    , DownloadJob { jobName = "sea"
                  , outputName = "sea.zip"
                  , tmpPath = tpath
                  , mvSrc = "sea.zip"
                  , mvDest = FP.append dataPath "sea.zip"
                  , sourceURL = T.concat
                                [ "http://osm2.pleiades.uni-wuppertal.de/sea/"
                                , "latest/sea.zip" ]
                  , unpackCmd = Nothing }

    , DownloadJob { jobName = "gmapi-builder"
                  , outputName = "gmapi-builder.tar.gz"
                  , tmpPath = tpath
                  , mvSrc = "gmapi-builder/gmapi-builder.py"
                  , mvDest = FP.append binPath "gmapi-builder.py"
                  , sourceURL = T.concat
                                [ "http://bitbucket.org/berteun/gmapibuilder/"
                                , "downloads/gmapi-builder.tar.gz" ]
                  , unpackCmd = Just "tar -xvzf gmapi-builder.tar.gz" }
    ]

  echo "Starting to split..."

  cd binPath
  splitOutputPath <- using (mktempdir tpath "split-output")

  let splitterCmd = T.concat [ "java -jar splitter/splitter.jar --output-dir="
                             , filepathToText splitOutputPath
                             , " "
                             , filepathToText dataPath
                             , "/ecuador-latest.osm.pbf"]

  echo $ T.unwords ["splitter command:", splitterCmd]

  shell splitterCmd empty

  mkgmapOutputPath <- using (mktempdir tpath "mkgmap-output")

  let mapName = T.concat [ "OSM ", country ]

  let mkgmapCmd = T.concat [ "java -jar mkgmap/mkgmap.jar"
                           , " --route"
                           , " --add-pois-to-areas"
                           , " --family-name=\"", mapName, "\""
                           , " --series-name=\"", mapName, "\""
                           , " --description=\"", mapName, "\""
                           , " --mapname=55500001"
                           , " --latin1"
                           , " --precomp-sea="
                           , filepathToText dataPath
                           , "/sea.zip"
                           , " --bounds="
                           , filepathToText dataPath
                           , "/bounds.zip"
                           , " --index"
                           , " --output-dir="
                           , filepathToText mkgmapOutputPath
                           , " --gmapsupp"
                           , " "
                           , filepathToText splitOutputPath
                           , "/*.osm.pbf"
                           ]

  echo "Starting to make map..."

  echo $ T.unwords ["mkgmap command:", mkgmapCmd]

  shell mkgmapCmd empty

  mv (FP.append mkgmapOutputPath "gmapsupp.img")
     (FP.append outputPath "gmapsupp.img")

  -- Now that we've removed the gmapsupp file, we can use the rest of
  -- the images from the previous step to create a map for Garmin
  -- basecamp.

  echo "Generated files from mkgmap:"
  view (ls mkgmapOutputPath)

  let gmapCmd = T.concat [ "./gmapi-builder.py -t "
                         , filepathToText mkgmapOutputPath
                         , "/osmmap.tdb"
                         , " -o "
                         , filepathToText outputPath
                         , " -b "
                         , filepathToText mkgmapOutputPath
                         , "/osmmap.img "
                         , filepathToText mkgmapOutputPath
                         , "/*.img"
                         ]

  echo $ T.unwords ["gmapi-builder command:", gmapCmd]

  shell gmapCmd empty

  echo $ T.unwords ["Output files copied to ", filepathToText outputPath]

  echo ""
  echo "Installation instructions:"
  echo ""
  echo $ T.concat [ "Your completed maps have been placed in "
                  , filepathToText outputPath, ":" ]
  echo $ T.concat [ "Basecamp map (to copy to ~/Library/Application\\ "
                  , "Support/Garmin/Maps/): ", filepathToText outputPath
                  , "/", mapName, ".gmapi/", mapName, ".gmap/"]

  echo $ T.concat [ "The map for installation in your Garmin device: "
                  , filepathToText outputPath, "/gmapsupp.img" ]

  echo ""

  echo "All done!"
