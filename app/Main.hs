module Main where

import qualified Lib (app1)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8081 Lib.app1
