module Main where

import Prelude hiding ((*>))

import Development.Shake

import Data.Diffusion.Common (URL(..), getEtag, curlCmd, getReqEnv)

opts :: ShakeOptions
opts = shakeOptions { shakeFiles  = ".shake/"
                    , shakeVerbosity = Diagnostic }

buildMap :: IO ()
buildMap = shakeArgs opts $ do
  etagOracle <- addOracle $ \(URL url) -> getEtag url
  
  want [".shake/ecuador.osm.pbf"]

  ".shake/ecuador.osm.pbf" *> \_ -> do
    need [ ".shake/south-america-latest.osm.pbf"
         , ".shake/south-america/ecuador.poly"
         , ".shake/osmosis/bin/osmosis"
         ]

    cmd Shell "./.shake/osmosis/bin/osmosis"
      [ "--read-pbf"
      , "file=\".shake/south-america-latest.osm.pbf\""
      , "--bounding-polygon"
      , "file=\".shake/south-america/ecuador.poly\""
      , "--write-pbf"
      , "file=\".shake/ecuador.osm.pbf\""
      ]

  "clean" ~> removeFilesAfter ".shake" ["//*"]

  ".shake/south-america-latest.osm.pbf" *> \f -> do
    purl <- getReqEnv "PLANET_URL"
    etagOracle $ URL purl
    curlCmd purl f

  ".shake/south-america/ecuador.poly" *> \f -> do
    curl <- getReqEnv "COUNTRY_POLYGON_URL"
    curlCmd curl f

  ".shake/osmosis/bin/osmosis" *> \_ -> do
    need [".shake/osmosis-latest.tgz"]
    cmd "tar" ["xvzf", ".shake/osmosis-latest.tgz", "-C", ".shake/osmosis"]
  
  ".shake/osmosis-latest.tgz" *> \f -> do
    ourl <- getReqEnv "OSMOSIS_URL"
    curlCmd ourl f

main :: IO ()
main = buildMap
