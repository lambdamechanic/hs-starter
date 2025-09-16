{-# LANGUAGE DuplicateRecordFields #-}

module Starter.OAuth.Types
  ( OAuthStartRequest (..)
  , OAuthStartResponse (..)
  , OAuthProfile (..)
  , OAuthCallbackRequest (..)
  , OAuthCallbackResponse (..)
  ) where

import Starter.Prelude

import Data.Aeson (FromJSON, ToJSON)

-- | Parameters required to initiate an OAuth login flow.
data OAuthStartRequest = OAuthStartRequest
  { redirectUri :: Text
  , scopes :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Response returned after staging an OAuth authorization redirect.
data OAuthStartResponse = OAuthStartResponse
  { state :: Text
  , codeVerifier :: Text
  , authorizationUrl :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Minimal profile data returned by an OAuth provider.
data OAuthProfile = OAuthProfile
  { subject :: Text
  , email :: Maybe Text
  , displayName :: Maybe Text
  , avatarUrl :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Callback payload delivered by the OAuth provider.
data OAuthCallbackRequest = OAuthCallbackRequest
  { state :: Text
  , code :: Text
  , profile :: OAuthProfile
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Response indicating whether the user may proceed with login.
data OAuthCallbackResponse = OAuthCallbackResponse
  { allowed :: Bool
  , redirectUri :: Text
  , userId :: Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
