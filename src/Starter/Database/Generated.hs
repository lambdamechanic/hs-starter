-- | This code was originally created by squealgen. Edit if you know how it got made and are willing to own it now.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Starter.Database.Generated where
import Squeal.PostgreSQL
import GHC.TypeLits(Symbol)

type PGltree = UnsafePGType "ltree"
type PGcidr = UnsafePGType "cidr"
type PGltxtquery = UnsafePGType "ltxtquery"
type PGlquery = UnsafePGType "lquery"


type DB = '["public" ::: Schema]

type Schema = Join Tables (Join Views (Join Enums (Join Functions (Join Composites Domains))))
-- enums

-- decls
type Enums =
  ('[] :: [(Symbol,SchemumType)])

type Composites =
  ('[] :: [(Symbol,SchemumType)])

-- schema
type Tables = ('[
   "oauth_login_events" ::: 'Table OauthLoginEventsTable
  ,"users" ::: 'Table UsersTable]  :: [(Symbol,SchemumType)])

-- defs
type OauthLoginEventsColumns = '["id" ::: 'Def :=> 'NotNull PGint4
  ,"issuer" ::: 'NoDef :=> 'NotNull PGtext
  ,"raw_payload" ::: 'NoDef :=> 'NotNull PGjsonb
  ,"logged_at" ::: 'Def :=> 'NotNull PGtimestamptz
  ,"user_id" ::: 'NoDef :=> 'NotNull PGint4
  ,"allowed" ::: 'Def :=> 'NotNull PGbool]
type OauthLoginEventsConstraints = '["oauth_login_events_pkey" ::: 'PrimaryKey '["id"]
  ,"oauth_login_events_user_id_fkey" ::: 'ForeignKey '["user_id"] "public" "users" '["id"]]
type OauthLoginEventsTable = OauthLoginEventsConstraints :=> OauthLoginEventsColumns

type UsersColumns = '["id" ::: 'Def :=> 'NotNull PGint4
  ,"email" ::: 'NoDef :=> 'Null PGtext
  ,"display_name" ::: 'NoDef :=> 'Null PGtext
  ,"avatar_url" ::: 'NoDef :=> 'Null PGtext
  ,"created_at" ::: 'Def :=> 'NotNull PGtimestamptz
  ,"updated_at" ::: 'Def :=> 'NotNull PGtimestamptz
  ,"uid" ::: 'NoDef :=> 'NotNull PGtext
  ,"issuer" ::: 'NoDef :=> 'NotNull PGtext
  ,"allowed" ::: 'Def :=> 'NotNull PGbool
  ,"last_login_at" ::: 'NoDef :=> 'Null PGtimestamptz]
type UsersConstraints = '["users_issuer_uid_key" ::: 'Unique '["issuer","uid"]
  ,"users_pkey" ::: 'PrimaryKey '["id"]]
type UsersTable = UsersConstraints :=> UsersColumns

-- VIEWS
type Views = 
  '[]


-- functions
type Functions = 
  '[  ]
type Domains = '[]

