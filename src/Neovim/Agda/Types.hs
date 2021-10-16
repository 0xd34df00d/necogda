module Neovim.Agda.Types where

import qualified Data.HashMap.Strict as HM
import UnliftIO
import UnliftIO.Process

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

defaultEnv :: MonadIO m => m AgdaEnv
defaultEnv = atomically $ AgdaEnv <$> newTVar mempty
