module Main where

-- Other requirements needed prior to running: harfbuzz. Need to install for
-- platform. For OS X, `brew install harfbuzz` works.

-- Note that you can make a bounding box on http://boundingbox.klokantech.com/.

import Prelude hiding ((*>))

import Development.Shake

import Data.Diffusion.Common (URL(..), getEtag, curlCmd, getReqEnv, dataDir)

opts :: ShakeOptions
opts = shakeOptions { shakeFiles     = dataDir
                    , shakeVerbosity = Diagnostic }

buildMap :: IO ()
buildMap = shakeArgs opts $ do
  etagOracle <- addOracle $ \(URL url) -> getEtag url
  
  want [".diffusion/map.pdf"]

  ".diffusion/ocitysmap/README" *> \_ -> do

    cmd Shell "git"
      [ "clone"
      , "--max-depth=1"
      , "https://github.com/maposmatic/ocitysmap.git"
      ]

  ".diffusion/ocitysmap/deps/mapbox/variant/README.md" *> \_ -> do
    cmd (Cwd ".diffusion/ocitysmap") "git submodule update" ["--init"]

  ".diffusion/ocitysmap/config.log" *> \_ -> do
    cmd (Cwd ".diffusion/ocitysmap") "./configure" []

  
  
  "clean" ~> removeFilesAfter ".diffusion" ["//*"]

main :: IO ()
main = buildMap
