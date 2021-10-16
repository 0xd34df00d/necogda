module Neovim.Agda.Types where

import qualified Data.HashMap.Strict as HM
import UnliftIO
import UnliftIO.Process

import Neovim

newtype NoShow a = NoShow { hidden :: a }

instance Show (NoShow a) where
  show _ = "hidden"

data AgdaInstance = AgdaInstance
  { agdaStdin :: Handle
  , agdaStdout :: Handle
  , agdaStderr :: Handle
  , agdaProcess :: NoShow ProcessHandle
  , filename :: String
  } deriving (Show)

newtype AgdaEnv = AgdaEnv
  { agdas :: TVar (HM.HashMap FilePath AgdaInstance)
  }

defaultEnv :: Neovim e AgdaEnv
defaultEnv = atomically $ AgdaEnv <$> newTVar mempty
