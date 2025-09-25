{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Starter.Database.Users
  ( upsertUser,
    insertLoginEvent,
    selectUserCount,
    DbUserRow (..),
  )
where

import Data.Aeson (Value)
import Data.Time (UTCTime)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Squeal.PostgreSQL
import Starter.Database.Schema (AppDb)
import Starter.Prelude hiding (from)

-- | User metadata returned from upserting the users table.
data DbUserRow = DbUserRow
  { userId :: Int32,
    userAllowed :: Bool
  }
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Upsert a user record based on a Firebase identity.
upsertUser :: Manipulation_ AppDb (Text, Text, Maybe Text, Maybe Text, Maybe Text, Bool, Maybe UTCTime, UTCTime) DbUserRow
upsertUser =
  insertInto
    (#public ! #users `as` #u)
    ( Values_
        ( Default
            `as` #id
            :* Set (param @3)
            `as` #email
            :* Set (param @4)
            `as` #display_name
            :* Set (param @5)
            `as` #avatar_url
            :* Default
            `as` #created_at
            :* Set (param @8)
            `as` #updated_at
            :* Set (param @2)
            `as` #uid
            :* Set (param @1)
            `as` #issuer
            :* Set (param @6)
            `as` #allowed
            :* Set (param @7)
            `as` #last_login_at
        )
    )
    ( OnConflict
        (OnConstraint #users_issuer_uid_key)
        ( DoUpdate
            ( Set (param @3)
                `as` #email
                :* Set (param @4)
                `as` #display_name
                :* Set (param @5)
                `as` #avatar_url
                :* Set (param @8)
                `as` #updated_at
                :* Set (param @6)
                `as` #allowed
                :* Set (param @7)
                `as` #last_login_at
                :* Nil
            )
            [ #u ! #issuer .== param @1,
              #u ! #uid .== param @2
            ]
        )
    )
    (Returning_ (#u ! #id `as` #userId :* #u ! #allowed `as` #userAllowed))

-- | Record a login attempt in the oauth_login_events audit table.
insertLoginEvent :: Manipulation_ AppDb (Int32, Text, Jsonb Value, Bool, UTCTime) ()
insertLoginEvent =
  insertInto
    (#public ! #oauth_login_events)
    ( Values_
        ( Default
            `as` #id
            :* Set (param @2)
            `as` #issuer
            :* Set (param @3)
            `as` #raw_payload
            :* Set (param @5)
            `as` #logged_at
            :* Set (param @1)
            `as` #user_id
            :* Set (param @4)
            `as` #allowed
        )
    )
    OnConflictDoRaise
    (Returning_ Nil)

-- | Count all users via an aggregate, requires `users` table to exist.
selectUserCount :: Statement AppDb () (Only Int64)
selectUserCount =
  query
    $ select_
      (countStar `as` #fromOnly)
      ( from (table (#public ! #users `as` #users))
          & groupBy Nil
      )
