{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket (
  Socket
  , socket
  , listen
  , bind
  , Family(AF_INET)
  , SocketType(Stream)
  , defaultProtocol
  , SockAddr(SockAddrInet)
  , iNADDR_ANY
  , PortNumber
  )

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
  runSettingsSocket
  , defaultSettings
  , setPort
  )

data Config = Config {
  portNumber :: PortNumber,
  initialModificationTime :: UTCTime,
  monitorPath :: FilePath
  } deriving (Eq, Show)


main :: IO ()
main = do
  config <- readConfig
  serverSocket <- socket AF_INET Stream defaultProtocol
  bind serverSocket (SockAddrInet (portNumber config) iNADDR_ANY)
  listen serverSocket 5
  let app = makeApp config serverSocket
  runSettingsSocket defaultSettings serverSocket app


makeApp :: Config -> Socket -> Application
makeApp config exitSocket =
  Lib.app1 exitSocket (initialModificationTime config) (monitorPath config)


readConfig :: IO Config
readConfig = do
  args <- Env.getArgs
  let textArgs = map Encoding.decodeUtf8 args
  let portArg = textArgs !! 0
  let Right (portNumber, _) = (Read.decimal portArg)

  let configPath = Text.unpack $ textArgs !! 1
  mtime <- getModificationTime configPath

  return $ Config (fromIntegral portNumber) mtime configPath
