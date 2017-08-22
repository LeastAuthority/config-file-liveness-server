{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (FilePath)
import System.Directory (getModificationTime)
import Data.Time (
  UTCTime
  )
import qualified System.Posix.Env.ByteString as Env (getArgs)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Encoding as Encoding (decodeUtf8)
import qualified Data.Text.Read as Read (decimal)

import qualified Lib (
  app1
  )
import Network.Wai (
  Application
  )
import Network.Wai.Handler.Warp (
  run
  )

data Config = Config {
  portNumber :: Int,
  initialModificationTime :: UTCTime,
  monitorPath :: FilePath
  } deriving (Eq, Show)


main :: IO ()
main = do
  config <- readConfig
  run (portNumber config) (makeApp config)


makeApp :: Config -> Application
makeApp config =
  Lib.app1 (initialModificationTime config) (monitorPath config)


readConfig :: IO Config
readConfig = do
  args <- Env.getArgs
  let textArgs = map Encoding.decodeUtf8 args
  let portArg = textArgs !! 0
  let Right (portNumber, _) = Read.decimal portArg

  let configPath = Text.unpack $ textArgs !! 1
  mtime <- getModificationTime configPath

  return $ Config portNumber mtime configPath
