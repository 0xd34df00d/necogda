{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Neovim.Agda
( defaultEnv
, loadNecogda

, startStandalone
) where

import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate.IsString
import Control.Monad
import Control.Monad.Reader
import System.IO (hGetLine, hPrint)
import UnliftIO
import UnliftIO.Process

import Neovim
import Neovim.API.String

import Neovim.Agda.Interaction
import Neovim.Agda.Types


sendCommand :: MonadIO m => AgdaInstance -> Interaction -> m ()
sendCommand AgdaInstance { agdaStdin, filename } int = liftIO $ hPrint agdaStdin $ IOTCM (AbsolutePath filename) NonInteractive Direct int

loadFile :: MonadIO m => AgdaInstance -> m ()
loadFile agda@AgdaInstance { filename } = sendCommand agda $ Cmd_load filename []

withWatcher :: MonadUnliftIO m => (m () -> m ()) -> m ()
withWatcher watch = do
  watcherInstance <- newEmptyMVar
  let stop = takeMVar watcherInstance >>= cancel
  watcher <- async $ forever $ watch stop
  putMVar watcherInstance watcher

hWaitWithEof :: MonadIO m => Handle -> Int -> m Bool
hWaitWithEof h t = hIsEOF h >>= \case True -> pure False
                                      False -> hWaitForInput h t

watchErrors :: MonadUnliftIO m => (String -> m ()) -> AgdaInstance -> m ()
watchErrors errHandler AgdaInstance { agdaStderr, agdaProcess } = withWatcher $ \stop -> do
  hasInput <- hWaitWithEof agdaStderr 1000
  exited <- getProcessExitCode $ hidden agdaProcess
  case (hasInput, exited) of
       (True, _)    -> liftIO (hGetLine agdaStderr) >>= errHandler
       (_, Nothing) -> pure ()
       (_, Just ec) -> do
         errHandler [i|Agda exited with error code: #{ec}|]
         stop

-- TODO this leaks green threads when agda exits
watchStdout :: MonadUnliftIO m => (String -> m ()) -> AgdaInstance -> m ()
watchStdout handler AgdaInstance { agdaStdout } = withWatcher $ \_ -> do
  hasInput <- hWaitWithEof agdaStdout 1000
  if hasInput
  then liftIO (hGetLine agdaStdout) >>= handler
  else pure ()

startAgdaForFile :: (MonadUnliftIO m, MonadReader AgdaEnv m, MonadFail m) => FilePath -> m (Maybe AgdaInstance)
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
  then do
    terminateProcess $ hidden agdaProcess
    pure Nothing
  else pure $ Just inst
  where
    agdaProc = (proc "agda" ["--interaction-json"]) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

startAgda :: Neovim AgdaEnv ()
startAgda = do
  buf <- vim_get_current_buffer
  name <- buffer_get_name buf

  agdasTVar <- asks agdas
  agdas <- readTVarIO agdasTVar
  unless (name `HM.member` agdas) $ do
    maybeInst <- startAgdaForFile name
    case maybeInst of
         Nothing -> pure ()
         Just inst -> do
           watchErrors nvim_err_writeln inst
           loadFile inst

loadNecogda :: Neovim AgdaEnv ()
loadNecogda = startAgda

startStandalone :: FilePath -> ReaderT AgdaEnv IO ()
startStandalone filename = do
  Just inst <- startAgdaForFile filename
  watchErrors (liftIO . putStrLn . ("[ERR] " <>)) inst
  loadFile inst
  watchStdout (liftIO . putStrLn) inst
