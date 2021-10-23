{-# LANGUAGE DeriveGeneric, DerivingStrategies, DerivingVia #-}

module Neovim.Agda.Response where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import GHC.Generics (Generic)

import Neovim.Agda.Response.AgdaJson

data StatusInfo = StatusInfo
  { checked :: Bool
  , showIrrelevantArguments :: Bool
  , showImplicitArguments :: Bool
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson StatusInfo)

data Position = Position
  { line :: Int
  , col :: Int
  , pos :: Int
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson Position)

data Range = Range
  { start :: Position
  , end :: Position
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson Range)

data RangeId = RangeId
  { range :: [Range]
  , id'range :: Int
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson RangeId)

data Goal = OfType
  { constraintObj :: RangeId
  , type'goal :: String
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson Goal)

data DisplayInfo = AllGoalsWarnings
  { visibleGoals :: [Goal]
  , warnings :: [()]
  , invisibleGoals :: [()]
  , errors :: [()]
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson DisplayInfo)

data HlBit = HlBit
  { atoms :: [String]
  , range'hlbit :: [Int64]
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson HlBit)

data HlInfo = HlInfo
  { remove :: Bool
  , payload :: [HlBit]
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson HlInfo)

data Response
  = Status { status :: StatusInfo }
  | InteractionPoints { interactionPoints :: [RangeId] }
  | DisplayInfo { info'd :: DisplayInfo }
  | HighlightingInfo { info'hl :: HlInfo }
  | RunningInfo { debugLevel :: Int, message :: String }
  | ClearRunningInfo
  | ClearHighlighting
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson Response)

decodeResponse :: BS.ByteString -> Either String Response
decodeResponse = A.eitherDecodeStrict'
