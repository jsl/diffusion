module Main where

import Prelude hiding ((*>))

import Development.Shake

import Data.Diffusion.Common (URL(..), getEtag, curlCmd, getReqEnv, dataDir)

opts :: ShakeOptions
opts = shakeOptions { shakeFiles     = dataDir
                    , shakeVerbosity = Diagnostic }

buildMap :: IO ()
buildMap = shakeArgs opts $ do
  etagOracle <- addOracle $ \(URL url) -> getEtag url

  want [".diffusion/gmapsupp.img"]

  ".diffusion/gmapsupp.img" *> \_ -> do
    need [ ".diffusion/mkgmap/dist/mkgmap.jar"
         , ".diffusion/split-output"
         , ".diffusion/map.osm.pbf"
         , ".diffusion/bounds.zip"
         , ".diffusion/sea.zip"
         , ".diffusion/style.zip"
         ]

    cmd Shell "java -jar .diffusion/mkgmap/dist/mkgmap.jar"
      [ "--route"               -- TODO - make all switches configurable
      , "--add-pois-to-areas"
      , "--latin1"
      , "--index"
      , "--gmapsupp"

      , "--family-name=\"OSM Map\""
      , "--series-name=\"OSM Map\""
      , "--description=\"OSM Map\""
      , "--precomp-sea=\".diffusion/sea.zip\""
      , "--bounds=\".diffusion/bounds.zip\""
      , "--output-dir=\".diffusion/\""
      , "--style-file=\".diffusion/style.zip\""
      , ".diffusion/split-output/*.osm.pbf"
      ]

  ".diffusion/split-output" *> \_ -> do
    need [ ".diffusion/splitter/dist/splitter.jar" ]
    cmd Shell "java" [ "-jar .diffusion/splitter/dist/splitter.jar"
                     , "--output-dir=.diffusion/split-output"
                     , ".diffusion/map.osm.pbf"
                     ]

  "clean" ~> removeFilesAfter ".diffusion" ["//*"]

  ".diffusion/bounds.zip" *> \f -> do
    url <- getReqEnv "BOUNDS_URL"
    etagOracle $ URL url
    curlCmd url f

  ".diffusion/style.zip" *> \f -> do
    url <- getReqEnv "STYLE_URL"
    etagOracle $ URL url
    curlCmd url f

  ".diffusion/map.osm.pbf" *> \f -> do
    url <- getReqEnv "MAP_URL"
    etagOracle $ URL url
    curlCmd url f

  ".diffusion/sea.zip" *> \f -> do
    url <- getReqEnv "SEA_URL"
    etagOracle $ URL url
    curlCmd url f

  ".diffusion/mkgmap/dist/mkgmap.jar" *> \_ -> do
    need [ ".diffusion/mkgmap/build.xml" ]
    cmd (Cwd ".diffusion/mkgmap") "ant"

  ".diffusion/mkgmap/build.xml" *> \_ ->
    cmd "svn" ["co", "http://svn.mkgmap.org.uk/mkgmap/trunk",  ".diffusion/mkgmap"]

  ".diffusion/splitter/build.xml" *> \_ ->
    cmd "svn" ["co", "http://svn.mkgmap.org.uk/splitter/trunk",  ".diffusion/splitter"]

  ".diffusion/splitter/dist/splitter.jar" *> \_ -> do
    need [".diffusion/splitter/build.xml"]
    cmd (Cwd ".diffusion/splitter") "ant"

  -- TODO - convert the gmapi-builder part to shake

  -- , DownloadJob
  --   { jobName = "style"
  --   , outputName = "default.zip"
  --   , mvSrc = "default.zip"
  --   , mvDest = dataP <> "default.zip"
  --   , sourceURL =
  --     "http://osm2gmap-styles.s3-website-us-east-1.amazonaws.com/default.zip"
  --   , unpackCmd = Nothing
  --   , checkForUpdate = True
  --   }

    -- let gmapCmd =
    --       "./gmapi-builder.py -t " <> filepathToText mkgmapOutputPath <>
    --       "/osmmap.tdb" <> " -o " <> filepathToText (outputPath paths) <>
    --       " -b " <> filepathToText mkgmapOutputPath <> "/osmmap.img " <>
    --       filepathToText mkgmapOutputPath <> "/*.img"

    -- echo $ "gmapi-builder command: " <> gmapCmd

main :: IO ()
main = buildMap
