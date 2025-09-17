{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- | This code was originally created by squealgen. Edit if you know how it got made and are willing to own it now.
module Starter.Database.Generated where

import GHC.TypeLits (Symbol)
import Squeal.PostgreSQL

type PGltree = UnsafePGType "ltree"

type PGcidr = UnsafePGType "cidr"

type PGltxtquery = UnsafePGType "ltxtquery"

type PGlquery = UnsafePGType "lquery"

type DB = '["public" ::: Schema]

type Schema = Join Tables (Join Views (Join Enums (Join Functions (Join Composites Domains))))

-- enums

-- decls
type Enums =
  ('[] :: [(Symbol, SchemumType)])

type Composites =
  ('[] :: [(Symbol, SchemumType)])

-- schema
type Tables =
  ( '[ "oauth_login_events" ::: 'Table OauthLoginEventsTable,
       "oauth_sessions" ::: 'Table OauthSessionsTable,
       "users" ::: 'Table UsersTable
     ] ::
      [(Symbol, SchemumType)]
  )

-- defs
type OauthLoginEventsColumns =
  '[ "id" ::: 'Def :=> 'NotNull PGint4,
     "provider" ::: 'NoDef :=> 'NotNull PGtext,
     "raw_payload" ::: 'NoDef :=> 'NotNull PGjsonb,
     "logged_at" ::: 'Def :=> 'NotNull PGtimestamptz,
     "user_id" ::: 'NoDef :=> 'NotNull PGint4,
     "allowed" ::: 'Def :=> 'NotNull PGbool
   ]

type OauthLoginEventsConstraints =
  '[ "oauth_login_events_pkey" ::: 'PrimaryKey '["id"],
     "oauth_login_events_user_id_fkey" ::: 'ForeignKey '["user_id"] "public" "users" '["id"]
   ]

type OauthLoginEventsTable = OauthLoginEventsConstraints :=> OauthLoginEventsColumns

type OauthSessionsColumns =
  '[ "state" ::: 'NoDef :=> 'NotNull PGtext,
     "code_verifier" ::: 'NoDef :=> 'NotNull PGtext,
     "provider" ::: 'NoDef :=> 'NotNull PGtext,
     "redirect_uri" ::: 'NoDef :=> 'NotNull PGtext,
     "requested_scopes" ::: 'NoDef :=> 'NotNull PGtext,
     "created_at" ::: 'Def :=> 'NotNull PGtimestamptz
   ]

type OauthSessionsConstraints = '["oauth_sessions_pkey" ::: 'PrimaryKey '["state"]]

type OauthSessionsTable = OauthSessionsConstraints :=> OauthSessionsColumns

type UsersColumns =
  '[ "id" ::: 'Def :=> 'NotNull PGint4,
     "email" ::: 'NoDef :=> 'Null PGtext,
     "display_name" ::: 'NoDef :=> 'Null PGtext,
     "avatar_url" ::: 'NoDef :=> 'Null PGtext,
     "created_at" ::: 'Def :=> 'NotNull PGtimestamptz,
     "updated_at" ::: 'Def :=> 'NotNull PGtimestamptz,
     "provider" ::: 'NoDef :=> 'NotNull PGtext,
     "subject" ::: 'NoDef :=> 'NotNull PGtext,
     "allowed" ::: 'Def :=> 'NotNull PGbool,
     "last_login_at" ::: 'NoDef :=> 'Null PGtimestamptz
   ]

type UsersConstraints =
  '[ "users_pkey" ::: 'PrimaryKey '["id"],
     "users_provider_subject_key" ::: 'Unique '["provider", "subject"]
   ]

type UsersTable = UsersConstraints :=> UsersColumns

-- VIEWS
type Views =
  '[]

-- functions
type Functions =
  '[]

type Domains = '[]
