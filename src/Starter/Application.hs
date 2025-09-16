module Starter.Application
  ( runApp
  , AppEnv (..)
  ) where

import Starter.Prelude

import Network.Wai.Handler.Warp (run)
import Starter.Server (app)

data AppEnv = AppEnv
  { appPort :: Int
  }

runApp :: IO ()
runApp = do
  let env = AppEnv {appPort = 8080}
  run (appPort env) (app env)
