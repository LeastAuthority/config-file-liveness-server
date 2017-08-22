{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (app1) where

import Control.Monad.IO.Class (liftIO)
import Data.Time (
  UTCTime
  )
import Data.Time.Clock.POSIX (
  posixSecondsToUTCTime
  )
import Data.Aeson (ToJSON)
import System.IO (FilePath)
import System.Directory (getModificationTime)
import Servant (
  Proxy(Proxy)
  , Server
  , serve
  , throwError
  , err500
  , errBody
  )
import Servant.API (
  Verb
  , Get
  , StdMethod(GET)
  , JSON
  , (:<|>)
  )
import Network.Wai (
  Application
  )
import GHC.Generics (Generic)

-- The data we'll return in a response (above and beyond the status code which
-- is what folks probably really care about).
data Liveness = Liveness {
  originalModified :: UTCTime,
  currentModified :: UTCTime
  } deriving (Eq, Show, Generic)

-- For symmetry: an alias for Get indicating aliveness (200 response).
type GetAlive = Get
-- A request resulting in a non-alive response.
type GetNotAlive = Verb 'GET 503 -- Service Unavailable

-- An HTTP endpoint at / that returns a JSON-encoded Liveness structure.
type LivenessProbeAPI1 = GetAlive '[JSON] Liveness
                    -- :<|> GetNotAlive '[JSON] Liveness

instance ToJSON Liveness

livenessServer1 :: UTCTime -> FilePath -> Server LivenessProbeAPI1
livenessServer1 initialModificationTime monitorPath = do
  mtime <- liftIO $ getModificationTime monitorPath
  case mtime == initialModificationTime of
    True  -> return $ Liveness initialModificationTime mtime
    False -> throwError $ err500 { errBody = "File modified." }

livenessProbeAPI :: Proxy LivenessProbeAPI1
livenessProbeAPI = Proxy

app1 :: UTCTime -> FilePath -> Application
app1 initialModificationTime monitorPath =
  serve livenessProbeAPI $ livenessServer1 initialModificationTime monitorPath
