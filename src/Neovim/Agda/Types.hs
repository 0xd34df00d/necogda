module Neovim.Agda.Types where

import qualified Data.HashMap.Strict as HM
import UnliftIO
import UnliftIO.Process

newtype NoShow a = NoShow { hidden :: a }

instance Show (NoShow a) where
  show _ = "hidden"

data AgdaInstanceT payload = AgdaInstance
  { agdaStdin :: Handle
  , agdaStdout :: Handle
  , agdaStderr :: Handle
  , agdaProcess :: NoShow ProcessHandle
  , filename :: String
  , payload :: payload
  } deriving (Show)

newtype AgdaEnvT payload = AgdaEnv
  { agdas :: TVar (HM.HashMap FilePath (AgdaInstanceT payload))
  }

defaultEnv :: MonadIO m => m (AgdaEnvT payload)
defaultEnv = atomically $ AgdaEnv <$> newTVar mempty
