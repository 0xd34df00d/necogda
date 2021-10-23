{-# LANGUAGE NamedFieldPuns #-}

module Neovim.Agda.Commands where

import System.IO (hPrint)
import UnliftIO

import Neovim.Agda.Interaction
import Neovim.Agda.Types

sendCommand :: MonadIO m => AgdaInstanceT payload -> Interaction -> m ()
sendCommand AgdaInstance { agdaStdin, filename } int = liftIO $ hPrint agdaStdin $ IOTCM (AbsolutePath filename) NonInteractive Direct int

loadFile :: MonadIO m => AgdaInstanceT payload -> m ()
loadFile agda@AgdaInstance { filename } = sendCommand agda $ Cmd_load filename []
