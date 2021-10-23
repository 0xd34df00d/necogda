{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Neovim.Agda.Response where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import GHC.Generics (Generic (Rep))

newtype AgdaJson a = AgdaJson { getAgdaJson :: a }
  deriving (Generic)

agdaJsonOpts :: A.Options
agdaJsonOpts = A.defaultOptions { A.sumEncoding = A.TaggedObject "kind" "", A.fieldLabelModifier = takeWhile (/= '\'') }

instance (A.GFromJSON A.Zero (Rep a), Generic a) => A.FromJSON (AgdaJson a) where
  parseJSON = fmap AgdaJson . A.genericParseJSON agdaJsonOpts

instance (A.GToJSON A.Zero (Rep a), Generic a) => A.ToJSON (AgdaJson a) where
  toJSON = A.genericToJSON agdaJsonOpts . getAgdaJson

data StatusInfo = StatusInfo
  { checked :: Bool
  , showIrrelevantArguments :: Bool
  , showImplicitArguments :: Bool
  }
  deriving (Show, Generic, A.FromJSON, A.ToJSON)

data DisplayInfo = AllGoalsWarnings
  { visibleGoals :: [()]
  , warnings :: [()]
  , invisibleGoals :: [()]
  , errors :: [()]
  }
  deriving (Show, Generic, A.FromJSON, A.ToJSON)

data HlBit = HlBit
  { atoms :: [String]
  , range :: [Int64]
  }
  deriving (Show, Generic, A.FromJSON, A.ToJSON)

data HlInfo = HlInfo
  { remove :: Bool
  , payload :: [HlBit]
  }
  deriving (Show, Generic, A.FromJSON, A.ToJSON)

data Response
  = Status { status :: StatusInfo }
  | InteractionPoints { interactionPoints :: [()] }
  | DisplayInfo { info'd :: DisplayInfo }
  | HighlightingInfo { info'hl :: HlInfo }
  | RunningInfo { debugLevel :: Int, message :: String }
  | ClearRunningInfo
  | ClearHighlighting
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson Response)

decodeResponse :: BS.ByteString -> Either String Response
decodeResponse = A.eitherDecodeStrict'
