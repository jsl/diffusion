module Main where

import Prelude hiding ((*>))

import Development.Shake

opts :: ShakeOptions
opts = shakeOptions { shakeFiles  = ".shake/" }

curlCmd :: String -> String -> Action ()
curlCmd url destfile = do
  cmd "curl" [url, "-s", "-o", destfile]

buildMap :: String -> String -> String -> IO ()
buildMap planetURL regionPolyURL osmosisURL = shakeArgs opts $ do
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
    --- cmd "curl" ["http://download.geofabrik.de/south-america-latest.osm.pbf", "-s", "-o", f]
    curlCmd planetURL f

  ".shake/south-america/ecuador.poly" *> \f -> do
    -- cmd "curl" ["http://download.geofabrik.de/south-america/ecuador.poly", "-s", "-o", f ]
    curlCmd regionPolyURL f

  ".shake/osmosis/bin/osmosis" *> \_ -> do
    need [".shake/osmosis-latest.tgz"]
    cmd "tar" ["xvzf", ".shake/osmosis-latest.tgz", "-C", ".shake/osmosis"]
  
  ".shake/osmosis-latest.tgz" *> \f -> do
    -- cmd "curl" ["http://bretth.dev.openstreetmap.org/osmosis-build/osmosis-latest.tgz", "-s", "-o", f]
    curlCmd osmosisURL f

main :: IO ()
main = buildMap
  "http://localhost:8000/south-america-latest.osm.pbf"
  "http://localhost:8000/ecuador.poly"
  "http://localhost:8000/osmosis-latest.tgz"
