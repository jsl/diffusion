{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module: Data.Diffusion.Common
Description: Common functions for the Diffusion program.
-}
module Data.Diffusion.Common where

import Development.Shake (Action(), getEnv, cmd, Exit(..), Stdout(..))

import Data.Hashable (Hashable())
import Data.Typeable (Typeable())
import Data.Binary (Binary())
import Control.DeepSeq (NFData)

import GHC.IO.Exception (ExitCode (ExitSuccess))

import Data.Time.Clock (getCurrentTime)

newtype URL = URL String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | Retrieves the given environment variable, or fails if
-- it is not found.
getReqEnv :: String -- ^ The environment variable to fetch
          -> Action String -- ^ The value of the given variable
getReqEnv var = do
  v <- getEnv var
  case v of
    Nothing -> fail $ "Error: required variable '" ++ var ++ "' was not supplied."
    Just v' -> return v'

-- | Fetches the given URL via Curl
curlCmd :: String -- ^ The URL to fetch
        -> String -- ^ The destination file
        -> Action ()
curlCmd url destfile = cmd "curl" [url, "-s", "-o", destfile]

-- | Fetches the ETag at the given URL. If no ETag is given by the server,
--  we use the UTC Time that the file was retrieved, so the file will be pretty
-- much downloaded every time.
getEtag :: String -- ^ The URL of the file
        -> IO String -- ^ The Etag or UTC Time that this function was called
getEtag url = do
  (Exit c, Stdout out) <- cmd $ "curl -I -L -s " ++ url ++ " | grep ETag"
  if c == ExitSuccess then
    return (out :: String)
   else
    do
      c' <- getCurrentTime
      return $ show c'
