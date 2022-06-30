{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Neovim.Agda.Start
( startAgda
, startAgdaForFile
, watchErrors
, watchStdout
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.Reader
import Data.Default
import Data.Functor
import Data.String.Interpolate.IsString
import UnliftIO
import UnliftIO.Process

import Neovim
import Neovim.API.ByteString

import Neovim.Agda.Request.Commands (loadFile)
import Neovim.Agda.Response.Dispatch
import Neovim.Agda.Types
import Neovim.Agda.Util

withWatcher :: MonadUnliftIO m => (m () -> m ()) -> m ()
withWatcher watch = do
  watcherInstance <- newEmptyMVar
  let stop = takeMVar watcherInstance >>= cancel
  watcher <- async $ forever $ watch stop
  putMVar watcherInstance watcher

hWaitWithEof :: MonadIO m => Handle -> Int -> m Bool
hWaitWithEof h t = hIsEOF h >>= \case True -> pure False
                                      False -> hWaitForInput h t

watchErrors :: MonadUnliftIO m => (BS.ByteString -> m ()) -> AgdaInstanceT payload -> m ()
watchErrors errHandler AgdaInstance { agdaStderr, agdaProcess } = withWatcher $ \stop -> do
  hasInput <- hWaitWithEof agdaStderr 1000
  exited <- getProcessExitCode $ hidden agdaProcess
  case (hasInput, exited) of
       (True, _)    -> liftIO (BS.hGetLine agdaStderr) >>= errHandler
       (_, Nothing) -> pure ()
       (_, Just ec) -> do
         errHandler [i|Agda exited with error code: #{ec}|]
         stop

-- TODO this leaks green threads when agda exits
watchStdout :: MonadUnliftIO m
            => (BS.ByteString -> m ())
            -> (BS.ByteString -> m ())
            -> AgdaInstanceT payload
            -> m ()
watchStdout handler errHandler AgdaInstance { agdaStdout } = withWatcher $ \_ -> do
  hasInput <- hWaitWithEof agdaStdout 1000
  if not hasInput
  then pure ()
  else do str <- liftIO (BS.hGetLine agdaStdout)
          handler str `catchAny` \e -> errHandler [i|`#{displayException e}` when handling #{T.decodeUtf8 str}|]

startAgdaForFile :: (MonadUnliftIO m, MonadReader (AgdaEnvT payload) m, MonadFail m)
                 => payload
                 -> FilePath
                 -> m (Maybe (AgdaInstanceT payload))
startAgdaForFile payload filename = do
  agdasTVar <- asks agdas
  (Just agdaStdin, Just agdaStdout, Just agdaStderr, NoShow -> agdaProcess) <- createProcess agdaProc
  hSetBuffering agdaStdin LineBuffering
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
  name <- T.unpack . T.decodeUtf8 <$> buffer_get_name buf

  goalWindowSize <- extractGoalWindowSize <$> nvim_eval [i|get(g:, 'necogda_goal_window_size', 60)|]

  nvim_command [i|vertical belowright pedit! Goals|]
  nvim_command [i|wincmd P|]
  nvim_command [i|#{goalWindowSize}wincmd >|]
  nvim_command [i|setlocal buftype=nofile nobuflisted bufhidden=wipe|]
  outputBuffer <- vim_get_current_buffer
  nvim_command [i|wincmd p|]

  let withPayload f = do
        agdasTVar <- asks agdas
        agdas <- readTVarIO agdasTVar
        case name `HM.lookup` agdas of
             Nothing -> nvim_err_writeln [i|Unable to find Agda instance for #{name}|] $> def
             Just inst -> f $ payload inst
  let modifyPayload f = do
        agdasTVar <- asks agdas
        atomically $ modifyTVar' agdasTVar $ HM.adjust (\inst -> inst { payload = f $ payload inst }) name

  let parseDispatch line = case parseResponse line of
                                Left errStr -> do nvim_err_writeln $ T.encodeUtf8 $ T.pack errStr
                                                  logToFile $ errStr <> "; when parsing\n" <> T.unpack (T.decodeUtf8 line)
                                Right resp  -> dispatchResponse (DispatchContext { agdaBuffer = buf, .. }) resp

  agdasTVar <- asks agdas
  agdas <- readTVarIO agdasTVar
  unless (name `HM.member` agdas) $ do
    maybeInst <- startAgdaForFile (NeovimPayload mempty mempty) name
    case maybeInst of
         Nothing -> pure ()
         Just inst -> do
           watchErrors nvim_err_writeln inst
           watchStdout parseDispatch nvim_err_writeln inst
           loadFile
  where
    extractGoalWindowSize :: Object -> Int
    extractGoalWindowSize (ObjectInt n) = fromIntegral n
    extractGoalWindowSize (ObjectUInt n) = fromIntegral n
    extractGoalWindowSize _ = 60
