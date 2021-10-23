{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Neovim.Agda.Response.AgdaJson(AgdaJson(..)) where

import qualified Data.Aeson as A
import GHC.Generics (Generic (Rep))

newtype AgdaJson a = AgdaJson { getAgdaJson :: a } deriving (Generic)

agdaJsonOpts :: A.Options
agdaJsonOpts = A.defaultOptions { A.sumEncoding = A.TaggedObject "kind" "", A.fieldLabelModifier = takeWhile (/= '\'') }

instance (A.GFromJSON A.Zero (Rep a), Generic a) => A.FromJSON (AgdaJson a) where
  parseJSON = fmap AgdaJson . A.genericParseJSON agdaJsonOpts

instance (A.GToJSON A.Zero (Rep a), Generic a) => A.ToJSON (AgdaJson a) where
  toJSON = A.genericToJSON agdaJsonOpts . getAgdaJson
