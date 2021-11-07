{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}

module Neovim.Agda.Interaction.Types where

import Data.Sequence (Seq)
import GHC.Generics

import Neovim
import Neovim.Classes

data Backend = MAlonzo deriving (Show)

data Rewrite = AsIs | Instantiated | HeadNormal | Simplified | Normalised deriving (Show, Read, Enum, Bounded, Generic, NFData)

instance NvimObject Rewrite where
  toObject = toObject . show
  fromObject = fmap read . fromObject

data PositionWithoutFile = Pn
  { srcFile :: ()
  , posPos :: Int32
  , posLine :: Int32
  , posCol :: Int32
  } deriving (Show)

data IntervalWithoutFile = Interval { iStart, iEnd :: PositionWithoutFile } deriving (Show)

newtype AbsolutePath = AbsolutePath String
  deriving newtype (Show)

newtype SrcFile = SrcFile (Maybe AbsolutePath)
  deriving newtype (Show)

data Range
  = NoRange
  | Range SrcFile (Seq IntervalWithoutFile)

instance Show Range where
  show NoRange = "noRange"
  show _ = error "Dunno how to handle this yet"

newtype InteractionId = InteractionId { interactionId :: Int }
  deriving newtype (Show)

{-# ANN module "HLint: ignore Use camelCase" #-}
data Interaction
  = Cmd_load                    FilePath [String]

  | Cmd_compile                 Backend FilePath [String]

  | Cmd_constraints

  | Cmd_metas

  | Cmd_show_module_contents_toplevel Rewrite String

  | Cmd_search_about_toplevel   Rewrite String

  | Cmd_infer_toplevel          Rewrite String

  | Cmd_compute_toplevel        Bool String

  | Cmd_highlight               InteractionId Range String

  | ShowImplicitArgs            Bool

  | ToggleImplicitArgs

  | Cmd_give                    InteractionId Range String

  | Cmd_refine                  InteractionId Range String

  | Cmd_intro                   Bool InteractionId Range String

  | Cmd_refine_or_intro         Bool InteractionId Range String

  | Cmd_autoOne                 InteractionId Range String

  | Cmd_helper_function         Rewrite InteractionId Range String

  | Cmd_infer                   Rewrite InteractionId Range String

  | Cmd_goal_type               Rewrite InteractionId Range String

  | Cmd_goal_type_context       Rewrite InteractionId Range String

  | Cmd_goal_type_context_infer Rewrite InteractionId Range String

  | Cmd_goal_type_context_check Rewrite InteractionId Range String

  | Cmd_show_module_contents    Rewrite InteractionId Range String

  | Cmd_make_case               InteractionId Range String

  | Cmd_compute                 Bool InteractionId Range String

  | Cmd_why_in_scope            InteractionId Range String

  | Cmd_why_in_scope_toplevel   String

  | Cmd_show_version
  deriving (Show)

data HighlightingLevel = None | NonInteractive | Interactive deriving (Show)
data HighlightingMethod = Direct | Indirect deriving (Show)

data IOTCM = IOTCM AbsolutePath HighlightingLevel HighlightingMethod Interaction
  deriving (Show)
