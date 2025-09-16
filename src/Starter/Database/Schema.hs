module Starter.Database.Schema
  ( AppSchema
  , AppDb
  ) where

import Squeal.PostgreSQL (Public)

import Starter.Database.Generated (AppSchema)

-- | Public database containing the application schema.
type AppDb = Public AppSchema
