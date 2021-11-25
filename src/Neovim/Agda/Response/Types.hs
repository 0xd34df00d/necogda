{-# LANGUAGE DeriveGeneric, DerivingVia, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Neovim.Agda.Response.Types where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Hashable ( Hashable )
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

newtype RangeId = RangeId { getId :: Int }
  deriving (Show, Eq)
  deriving newtype (A.FromJSON, A.ToJSON, Hashable)

data RangeWithId = RangeWithId
  { range :: [Range]
  , id'range :: RangeId
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson RangeWithId)

data RangeWithName = RangeWithName
  { range :: [Range]
  , name'range :: T.Text
  }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson RangeWithName)

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

data TypeAux
  = GoalOnly
  | GoalAndHave { expr :: T.Text }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson TypeAux)

data GoalInfo
  = GoalType
    { type'goal :: T.Text
    , entries :: [GoalContextEntry]
    , typeAux :: Maybe TypeAux
    }
  | CurrentGoal { type'goal :: T.Text }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson GoalInfo)

data DisplayInfo
  = AllGoalsWarnings
    { visibleGoals :: [Goal RangeWithId]
    , warnings :: [()]
    , invisibleGoals :: [Goal RangeWithName]
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

newtype GiveResult = GiveResult { str'given :: T.Text }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson GiveResult)

data Response
  = Status { status :: StatusInfo }
  | InteractionPoints { interactionPoints :: [RangeWithId] }
  | DisplayInfo { info'd :: DisplayInfo }
  | GiveAction { giveResult :: GiveResult, interactionPoint :: RangeWithId }
  | MakeCase { variant :: String, clauses :: [T.Text], interactionPoint :: RangeWithId }
  | HighlightingInfo { info'hl :: HlInfo }
  | RunningInfo { debugLevel :: Int, message :: T.Text }
  | ClearRunningInfo
  | ClearHighlighting
  | JumpToError { filepath :: T.Text, position :: Int }
  deriving (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via (AgdaJson Response)

decodeResponse :: BS.ByteString -> Either String Response
decodeResponse = A.eitherDecodeStrict'
