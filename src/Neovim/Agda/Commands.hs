{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Neovim.Agda.Commands
( sendCommand

, loadFile
, goalInfo
) where

import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate.IsString
import System.IO (hPrint)
import UnliftIO

import Neovim
import Neovim.API.String

import Neovim.Agda.Interaction
import Neovim.Agda.Types

sendCommand :: MonadIO m => AgdaInstanceT payload -> Interaction -> m ()
sendCommand AgdaInstance { agdaStdin, filename } int = liftIO $ hPrint agdaStdin $ IOTCM (AbsolutePath filename) NonInteractive Direct int

withInstance :: (AgdaInstanceT payload -> Neovim (AgdaEnvT payload) ()) -> Neovim (AgdaEnvT payload) ()
withInstance cmd = do
  buf <- nvim_get_current_buf
  name <- buffer_get_name buf
  agdasTVar <- asks agdas
  agdas <- readTVarIO agdasTVar
  maybe (pure ()) cmd $ HM.lookup name agdas

loadFile :: Neovim (AgdaEnvT payload) ()
loadFile = withInstance $ \agda -> sendCommand agda $ Cmd_load (filename agda) []

goalInfo :: Neovim AgdaEnv ()
goalInfo = withInstance $ \agda -> do
  maybeInteractionId <- getCurrentInteractionId agda
  case maybeInteractionId of
       Nothing -> nvim_err_writeln [i|Not in an interaction point|]
       Just iid -> sendCommand agda $ Cmd_goal_type_context Simplified iid NoRange ""
