{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Neovim.Agda
( defaultEnv
, loadNecogda
) where

import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate.IsString
import Control.Monad
import System.IO (hGetLine, hPrint)
import UnliftIO
import UnliftIO.Process

import Neovim
import Neovim.API.String

import Neovim.Agda.Interaction
import Neovim.Agda.Types


sendCommand :: MonadIO m => AgdaInstance -> Interaction -> m ()
sendCommand AgdaInstance { agdaStdin, filename } int = liftIO $ hPrint agdaStdin $ IOTCM (AbsolutePath filename) NonInteractive Indirect int

loadFile :: MonadIO m => AgdaInstance -> m ()
loadFile agda@AgdaInstance { filename } = sendCommand agda $ Cmd_load filename []

watchErrors :: AgdaInstance -> Neovim AgdaEnv ()
watchErrors AgdaInstance { agdaStderr, agdaProcess } = do
  watcherInstance <- newEmptyMVar
  watcher <- async $ forever do
    hasInput <- hIsEOF agdaStderr >>= \case True -> pure False
                                            False -> hWaitForInput agdaStderr 1000
    exited <- getProcessExitCode $ hidden agdaProcess
    case (hasInput, exited) of
         (True, _)    -> liftIO (hGetLine agdaStderr) >>= nvim_err_writeln
         (_, Nothing) -> pure ()
         (_, Just ec) -> do
           nvim_err_writeln [i|Agda exited with error code: #{ec}|]
           watcher <- takeMVar watcherInstance
           cancel watcher
  putMVar watcherInstance watcher

startAgdaForFile :: FilePath -> Neovim AgdaEnv ()
startAgdaForFile filename = do
  agdasTVar <- asks agdas
  (Just agdaStdin, Just agdaStdout, Just agdaStderr, NoShow -> agdaProcess) <- createProcess agdaProc
  let inst = AgdaInstance { .. }
  shouldClose <- atomically do
    agdas <- readTVar agdasTVar
    if filename `HM.member` agdas
    then pure True
    else do
      modifyTVar' agdasTVar $ HM.insert filename inst
      pure False
  if shouldClose
  then terminateProcess $ hidden agdaProcess
  else watchErrors inst

  loadFile inst
  where
    agdaProc = (proc "agda" ["--interaction-json"]) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

startAgda :: Neovim AgdaEnv ()
startAgda = do
  buf <- vim_get_current_buffer
  name <- buffer_get_name buf

  agdasTVar <- asks agdas
  agdas <- readTVarIO agdasTVar
  unless (name `HM.member` agdas) $ startAgdaForFile name

loadNecogda :: Neovim AgdaEnv ()
loadNecogda = startAgda
