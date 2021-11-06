module Neovim.Agda.Types where

import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import UnliftIO
import UnliftIO.Process

import Neovim.Agda.Response

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

data AgdaEnvT payload = AgdaEnv
  { agdas :: TVar (HM.HashMap FilePath (AgdaInstanceT payload))

  , symbolInputCol :: TVar (Maybe Int)

  , goalmarksNs :: TVar Int64
  }

defaultEnv :: MonadIO m => m (AgdaEnvT payload)
defaultEnv = atomically $ AgdaEnv <$> newTVar mempty
                                  <*> newTVar Nothing
                                  <*> newTVar (-1)

type MarkId2InteractionPoint = HM.HashMap Int64 RangeId
type InteractionPoint2MarkIds = HM.HashMap RangeId [Int64]

data NeovimPayload = NeovimPayload
  { markId2interactionPoint  :: MarkId2InteractionPoint
  , interactionPoint2markIds :: InteractionPoint2MarkIds
  } deriving (Show)

type AgdaInstance = AgdaInstanceT NeovimPayload
type AgdaEnv = AgdaEnvT NeovimPayload
