{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib (app1) where

import Data.Time (
  UTCTime
  )
import Data.Time.Clock.POSIX (
  posixSecondsToUTCTime
  )
import Data.Aeson (ToJSON)
import Servant (
  Proxy(Proxy)
  , Server
  , serve
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

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

livenessServer1 :: UTCTime -> Server LivenessProbeAPI1
livenessServer1 initialModificationTime = return (
  Liveness initialModificationTime (posixSecondsToUTCTime 2)
  )

livenessProbeAPI :: Proxy LivenessProbeAPI1
livenessProbeAPI = Proxy

app1 :: UTCTime -> Application
app1 initialModificationTime =
  serve livenessProbeAPI $ livenessServer1 initialModificationTime
