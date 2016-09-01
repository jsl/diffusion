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

  want [".diffusion/sub-map.osm.pbf"]

  ".diffusion/sub-map.osm.pbf" *> \_ -> do
    need [ ".diffusion/full-map.osm.pbf"
         , ".diffusion/region.poly"
         , ".diffusion/osmosis/bin/osmosis"
         ]

    cmd Shell "./.diffusion/osmosis/bin/osmosis"
      [ "--read-pbf"
      , "file=\".diffusion/full-map.osm.pbf\""
      , "--bounding-polygon"
      , "file=\".diffusion/region.poly\""
      , "--write-pbf"
      , "file=\".diffusion/sub-map.osm.pbf\""
      ]

  "clean" ~> removeFilesAfter ".diffusion" ["//*"]

  ".diffusion/full-map.osm.pbf" *> \f -> do
    purl <- getReqEnv "FULL_MAP_URL"
    etagOracle $ URL purl
    curlCmd purl f

  ".diffusion/region.poly" *> \f -> do
    curl <- getReqEnv "REGION_POLYGON_URL"
    curlCmd curl f

  ".diffusion/osmosis/bin/osmosis" *> \_ -> do
    need [".diffusion/osmosis-latest.tgz"]
    cmd "tar" ["xvzf", ".diffusion/osmosis-latest.tgz", "-C", ".diffusion/osmosis"]

  ".diffusion/osmosis-latest.tgz" *> \f -> do
    ourl <- getReqEnv "OSMOSIS_URL"
    curlCmd ourl f

main :: IO ()
main = buildMap
