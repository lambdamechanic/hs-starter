module Starter.Env
  ( AppEnv (..),
  )
where

import Starter.Auth.Firebase (FirebaseAuth, FirebaseUser)
import Starter.Auth.Session (SessionConfig)
import Starter.Database.Connection (DbConfig)
import Starter.Prelude

-- | Global environment values shared across the application runtime.
data AppEnv = AppEnv
  { appPort :: Int,
    otelServiceName :: Text,
    otelCollectorEndpoint :: Maybe String,
    otelCollectorHeaders :: Maybe String,
    dbConfig :: DbConfig,
    authorizeLogin :: FirebaseUser -> IO Bool,
    firebaseAuth :: FirebaseAuth,
    sessionConfig :: SessionConfig,
    frontendDir :: FilePath
  }
