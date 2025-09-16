{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Starter.Database.OAuth
  ( insertSession
  , selectSession
  , deleteSession
  , upsertUser
  , insertLoginEvent
  , OAuthSessionRow (..)
  , DbUserRow (..)
  ) where

import Starter.Prelude hiding (from)

import Data.Aeson (Value)
import Data.Time (UTCTime)
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL

import Starter.Database.Schema (AppDb)

-- | Representation of a staged OAuth session stored in Postgres.
data OAuthSessionRow = OAuthSessionRow
  { sessionState :: Text
  , sessionCodeVerifier :: Text
  , sessionProvider :: Text
  , sessionRedirectUri :: Text
  , sessionRequestedScopes :: Text
  }
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | User metadata returned from upserting the users table.
data DbUserRow = DbUserRow
  { userId :: Int32
  , userAllowed :: Bool
  }
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Persist a new OAuth session for the client.
insertSession :: Manipulation_ AppDb (Text, Text, Text, Text, Text) ()
insertSession =
  insertInto (#public ! #oauth_sessions)
    ( Values_
        ( Set (param @1) `as` #state
            :* Set (param @2) `as` #code_verifier
            :* Set (param @3) `as` #provider
            :* Set (param @4) `as` #redirect_uri
            :* Set (param @5) `as` #requested_scopes
            :* Default `as` #created_at
        )
    )
    OnConflictDoRaise
    (Returning_ Nil)

-- | Fetch a stored OAuth session by state handle.
selectSession :: Statement AppDb (Only Text) OAuthSessionRow
selectSession =
  query $
    select_
      ( #oauth_sessions ! #state `as` #sessionState
          :* #oauth_sessions ! #code_verifier `as` #sessionCodeVerifier
          :* #oauth_sessions ! #provider `as` #sessionProvider
          :* #oauth_sessions ! #redirect_uri `as` #sessionRedirectUri
          :* #oauth_sessions ! #requested_scopes `as` #sessionRequestedScopes
      )
      ( from (table (#public ! #oauth_sessions `as` #oauth_sessions))
          & where_ (#oauth_sessions ! #state .== param @1)
      )

-- | Clear an OAuth session once it has been consumed.
deleteSession :: Manipulation_ AppDb (Only Text) ()
deleteSession =
  deleteFrom #oauth_sessions NoUsing (#state .== param @1) (Returning_ Nil)

-- | Upsert a user record based on the OAuth identity.
upsertUser :: Manipulation_ AppDb (Text, Text, Maybe Text, Maybe Text, Maybe Text, Bool, Maybe UTCTime, UTCTime) DbUserRow
upsertUser =
  insertInto (#public ! #users `as` #u)
    ( Values_
        ( Default `as` #id
            :* Set (param @3) `as` #email
            :* Set (param @4) `as` #display_name
            :* Set (param @5) `as` #avatar_url
            :* Default `as` #created_at
            :* Set (param @8) `as` #updated_at
            :* Set (param @1) `as` #provider
            :* Set (param @2) `as` #subject
            :* Set (param @6) `as` #allowed
            :* Set (param @7) `as` #last_login_at
        )
    )
    ( OnConflict (OnConstraint #users_provider_subject_key)
        ( DoUpdate
            ( Set (param @3) `as` #email
                :* Set (param @4) `as` #display_name
                :* Set (param @5) `as` #avatar_url
                :* Set (param @8) `as` #updated_at
                :* Set (param @6) `as` #allowed
                :* Set (param @7) `as` #last_login_at
                :* Nil
            )
            [ #u ! #provider .== param @1
            , #u ! #subject .== param @2
            ]
        )
    )
    ( Returning_ (#u ! #id `as` #userId :* #u ! #allowed `as` #userAllowed)
    )

-- | Record a login attempt in the oauth_login_events audit table.
insertLoginEvent :: Manipulation_ AppDb (Int32, Text, Jsonb Value, Bool, UTCTime) ()
insertLoginEvent =
  insertInto (#public ! #oauth_login_events)
    ( Values_
        ( Default `as` #id
            :* Set (param @2) `as` #provider
            :* Set (param @3) `as` #raw_payload
            :* Set (param @5) `as` #logged_at
            :* Set (param @1) `as` #user_id
            :* Set (param @4) `as` #allowed
        )
    )
    OnConflictDoRaise
    (Returning_ Nil)
