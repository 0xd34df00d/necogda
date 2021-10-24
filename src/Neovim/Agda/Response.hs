{-# LANGUAGE DeriveGeneric, DerivingStrategies, DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Neovim.Agda.Response where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
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
  { line :: Int64
  , col :: Int64
  , pos :: Int64
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

data RangeName = RangeName
  { range :: [Range]
  , name'range :: T.Text
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson RangeName)

data Goal range
  = OfType
    { constraintObj :: range
    , type'goal :: T.Text
    }
  | JustSort { constraintObj :: range }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson (Goal range))

newtype ErrorObj = ErrorObj { message'error :: T.Text }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson ErrorObj)

data GoalContextEntry = GoalContextEntry
  { inScope :: Bool
  , originalName :: T.Text
  , reifiedName :: T.Text
  , binding :: T.Text
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson GoalContextEntry)

data GoalInfo = GoalInfo
  { type'goal :: T.Text
  , entries :: [GoalContextEntry]
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson GoalInfo)

data DisplayInfo
  = AllGoalsWarnings
    { visibleGoals :: [Goal RangeId]
    , warnings :: [()]
    , invisibleGoals :: [Goal RangeName]
    , errors :: [()]
    }
  | Version { version :: T.Text }
  | Error
    { error' :: ErrorObj
    , warnings :: [()]
    }
  | GoalSpecific { goalInfo :: GoalInfo }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson DisplayInfo)

data HlBit = HlBit
  { atoms :: [T.Text]
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
  | RunningInfo { debugLevel :: Int, message :: T.Text }
  | ClearRunningInfo
  | ClearHighlighting
  | JumpToError { filepath :: T.Text, position :: Int }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson Response)

decodeResponse :: BS.ByteString -> Either String Response
decodeResponse = A.eitherDecodeStrict'
