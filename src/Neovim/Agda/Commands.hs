{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Neovim.Agda.Commands
( sendCommand

, loadFile
) where

import qualified Data.HashMap.Strict as HM
import System.IO (hPrint)
import UnliftIO

import Neovim
import Neovim.API.String

import Neovim.Agda.Interaction
import Neovim.Agda.Types

sendCommand :: MonadIO m => AgdaInstanceT payload -> Interaction -> m ()
sendCommand AgdaInstance { agdaStdin, filename } int = liftIO $ hPrint agdaStdin $ IOTCM (AbsolutePath filename) NonInteractive Direct int

withInstance :: (forall m. MonadIO m => AgdaInstanceT payload -> m ()) -> Neovim (AgdaEnvT payload) ()
withInstance cmd = do
  buf <- nvim_get_current_buf
  name <- buffer_get_name buf
  agdasTVar <- asks agdas
  agdas <- readTVarIO agdasTVar
  maybe (pure ()) cmd $ HM.lookup name agdas

loadFile :: Neovim (AgdaEnvT payload) ()
loadFile = withInstance $ \agda -> sendCommand agda $ Cmd_load (filename agda) []
