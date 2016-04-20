module Main where

import Prelude hiding ((*>))

import Development.Shake

import Control.Monad (liftM)

import Data.Diffusion.Common (URL(..), getEtag, curlCmd)

opts :: ShakeOptions
opts = shakeOptions { shakeFiles  = ".shake/"
                    , shakeVerbosity = Diagnostic }

vars :: [String]
vars = [ "MAP_URL"
       , "BOUNDS_URL"
       , "SEA_URL"
       , "STYLE_URL"
       ]

data Options = Options
 { mapURL :: String
 , boundsURL :: String
 , seaURL :: String
 , styleURL :: String
 }

getOptions :: Action Options
getOptions = do
  let opts  = mapM getEnv vars
      opts' = liftM sequence opts

  opts'' <- opts'

  case opts'' of
    Just xs -> return $ toOpts xs
    Nothing -> fail "Failed to set a required parameter for build!"

  where toOpts [ mapUrl
               , boundsUrl
               , seaUrl
               , styleUrl
               ] = Options mapUrl boundsUrl seaUrl styleUrl

buildMap :: IO ()
buildMap = shakeArgs opts $ do
  let opts = getOptions

  etagOracle <- addOracle $ \(URL url) -> liftIO $ getEtag url

  want [".osm2gmap/gmapsupp.img"]

  ".osm2gmap/gmapsupp.img" *> \_ -> do
    need [ ".osm2gmap/mkgmap/dist/mkgmap.jar"
         , ".osm2gmap/split-output"
         , ".osm2gmap/map.osm.pbf"
         , ".osm2gmap/bounds.zip"
         , ".osm2gmap/sea.zip"
         , ".osm2gmap/style.zip"
         ]

    cmd Shell "java -jar .osm2gmap/mkgmap/dist/mkgmap.jar"
      [ "--route"               -- TODO - make all switches configurable
      , "--add-pois-to-areas"
      , "--latin1"
      , "--index"
      , "--gmapsupp"

      , "--family-name=\"OSM Map\""
      , "--series-name=\"OSM Map\""
      , "--description=\"OSM Map\""
      , "--precomp-sea=\".osm2gmap/sea.zip\""
      , "--bounds=\".osm2gmap/bounds.zip\""
      , "--output-dir=\".osm2gmap/\""
      , "--style-file=\".osm2gmap/style.zip\""
      , ".osm2gmap/split-output/*.osm.pbf"
      ]

  ".osm2gmap/split-output" *> \_ -> do
    need [ ".osm2gmap/splitter/dist/splitter.jar" ]
    cmd Shell "java" [ "-jar .osm2gmap/splitter/dist/splitter.jar"
                     , "--output-dir=.osm2gmap/split-output"
                     , ".osm2gmap/map.osm.pbf"
                     ]

  "clean" ~> removeFilesAfter ".osm2gmap" ["//*"]

  ".osm2gmap/bounds.zip" *> \f -> do
    url <- liftM boundsURL opts
    etagOracle $ URL url
    curlCmd url f

  ".osm2gmap/style.zip" *> \f -> do
    url <- liftM styleURL opts
    etagOracle $ URL url
    curlCmd url f

  ".osm2gmap/map.osm.pbf" *> \f -> do
    url <- liftM mapURL opts
    etagOracle $ URL url
    curlCmd url f

  ".osm2gmap/sea.zip" *> \f -> do
    url <- liftM seaURL opts
    etagOracle $ URL url
    curlCmd url f

  ".osm2gmap/mkgmap/dist/mkgmap.jar" *> \_ -> do
    need [ ".osm2gmap/mkgmap/build.xml" ]
    cmd (Cwd ".osm2gmap/mkgmap") "ant"

  ".osm2gmap/mkgmap/build.xml" *> \_ ->
    cmd "svn" ["co", "http://svn.mkgmap.org.uk/mkgmap/trunk",  ".osm2gmap/mkgmap"]

  ".osm2gmap/splitter/build.xml" *> \_ ->
    cmd "svn" ["co", "http://svn.mkgmap.org.uk/splitter/trunk",  ".osm2gmap/splitter"]

  ".osm2gmap/splitter/dist/splitter.jar" *> \_ -> do
    need [".osm2gmap/splitter/build.xml"]
    cmd (Cwd ".osm2gmap/splitter") "ant"

main :: IO ()
main = buildMap
