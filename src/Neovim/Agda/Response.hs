{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Neovim.Agda.Response where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import GHC.Generics (Generic)

data StatusInfo = StatusInfo
  { checked :: Bool
  , showIrrelevantArguments :: Bool
  , showImplicitArguments :: Bool
  } deriving (Show, Generic, A.FromJSON, A.ToJSON)

data DisplayInfo = AllGoalsWarnings
  { visibleGoals :: [()]
  , warnings :: [()]
  , invisibleGoals :: [()]
  , errors :: [()]
  } deriving (Show, Generic)

instance A.FromJSON DisplayInfo where
  parseJSON = A.genericParseJSON A.defaultOptions { A.sumEncoding = A.TaggedObject "kind" "" }

instance A.ToJSON DisplayInfo where
  toJSON = A.genericToJSON A.defaultOptions { A.sumEncoding = A.TaggedObject "kind" "" }

data HlBit = HlBit
  { atoms :: [String]
  , range :: [Int64]
  } deriving (Show, Generic, A.FromJSON, A.ToJSON)

data HlInfo = HlInfo
  { remove :: Bool
  , payload :: [HlBit]
  } deriving (Show, Generic, A.FromJSON, A.ToJSON)

data Response
  = Status { status :: StatusInfo }
  | InteractionPoints { interactionPoints :: [()] }
  | DisplayInfo { info'd :: DisplayInfo }
  | HighlightingInfo { info'hl :: HlInfo }
  | RunningInfo { debugLevel :: Int, message :: String }
  | ClearRunningInfo
  | ClearHighlighting
  deriving (Show, Generic)

instance A.FromJSON Response where
  parseJSON = A.genericParseJSON A.defaultOptions { A.sumEncoding = A.TaggedObject "kind" "", A.fieldLabelModifier = takeWhile (/= '\'') }

instance A.ToJSON Response where
  toJSON = A.genericToJSON A.defaultOptions { A.sumEncoding = A.TaggedObject "kind" "", A.fieldLabelModifier = takeWhile (/= '\'') }

decodeResponse :: BS.ByteString -> Either String Response
decodeResponse = A.eitherDecodeStrict'
