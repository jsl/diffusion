{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding ((*>))

import Development.Shake

import Data.Hashable (Hashable())
import Data.Typeable (Typeable())
import Data.Binary (Binary())
import Control.DeepSeq (NFData)

import Control.Monad (liftM)


opts :: ShakeOptions
opts = shakeOptions { shakeFiles  = ".shake/"
                    , shakeVerbosity = Diagnostic }

curlCmd :: String -> String -> Action ()
curlCmd url destfile = do
  cmd "curl" [url, "-s", "-o", destfile]

data Options = Options
  { planetURL         :: String
  , countryPolygonURL :: String
  , osmosisURL        :: String
  }

newtype URL = URL String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

getOptions :: Action Options
getOptions = do
  let opts  = mapM getEnv ["PLANET_URL", "COUNTRY_POLYGON_URL", "OSMOSIS_URL"]
      opts' = liftM sequence opts

  opts'' <- opts'
  
  case opts'' of
    Just xs -> return $ toOpts xs
    Nothing -> fail "Failed to set a required parameter for build!"
    
  where toOpts [purl, curl, ourl] = Options purl curl ourl

buildMap :: IO ()
buildMap = shakeArgs opts $ do
  let opts = getOptions

  getEtag <- addOracle $ \(URL url) -> do
    Stdout out <- cmd $ "curl -I -L -s " ++ url ++ " | grep ETag | tail -n 1"
    return (out :: String)
  
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
    purl <- liftM planetURL opts
    getEtag $ URL purl
    curlCmd purl f

  ".shake/south-america/ecuador.poly" *> \f -> do
    curl <- liftM countryPolygonURL opts
    curlCmd curl f

  ".shake/osmosis/bin/osmosis" *> \_ -> do
    need [".shake/osmosis-latest.tgz"]
    cmd "tar" ["xvzf", ".shake/osmosis-latest.tgz", "-C", ".shake/osmosis"]
  
  ".shake/osmosis-latest.tgz" *> \f -> do
    ourl <- liftM osmosisURL opts
    curlCmd ourl f

main :: IO ()
main = buildMap
