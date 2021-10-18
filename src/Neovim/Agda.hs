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
import qualified Data.ByteString.Char8 as BS
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Data.Foldable
import Data.List
import Data.String.Interpolate.IsString
import System.IO (hPrint)
import UnliftIO
import UnliftIO.Process

import Neovim
import Neovim.API.ByteString

import Neovim.Agda.Interaction
import Neovim.Agda.Response
import Neovim.Agda.Types

type AgdaEnv = AgdaEnvT ()

sendCommand :: MonadIO m => AgdaInstanceT payload -> Interaction -> m ()
sendCommand AgdaInstance { agdaStdin, filename } int = liftIO $ hPrint agdaStdin $ IOTCM (AbsolutePath filename) NonInteractive Direct int

loadFile :: MonadIO m => AgdaInstanceT payload -> m ()
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
watchStdout :: MonadUnliftIO m => (BS.ByteString -> m ()) -> AgdaInstanceT payload -> m ()
watchStdout handler AgdaInstance { agdaStdout } = withWatcher $ \_ -> do
  hasInput <- hWaitWithEof agdaStdout 1000
  if hasInput
  then liftIO (BS.hGetLine agdaStdout) >>= handler
  else pure ()

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

stripMarker :: BS.ByteString -> BS.ByteString
stripMarker str
  | marker `BS.isPrefixOf` str = BS.drop (BS.length marker) str
  | otherwise = str
  where
    marker = "JSON> "

parseResponse :: Buffer -> BS.ByteString -> Neovim AgdaEnv ()
parseResponse buf response =
  case decodeResponse $ stripMarker response of
       Left errStr -> nvim_err_writeln $ BS.pack errStr
       Right res -> dispatchResponse buf res

addHlBit :: Buffer -> [Int64] -> HlBit -> Neovim AgdaEnv ()
addHlBit buf offsets (HlBit atoms [from, to])
  | Just fromLine <- subtract 1 <$> findIndex (>= from) offsets
  , Just toLine <- subtract 1 <$> findIndex (>= to) offsets
    = let (fromOffset, toOffset) = join (***) (offsets !!) (fromLine, toLine)
          (fromLine', toLine') = join (***) fromIntegral (fromLine, toLine)
      in if fromLine' == toLine'
         then forM_ atoms $ \atom ->
            nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] fromLine' (from - fromOffset - 1) (to - toOffset - 1)
         else forM_ atoms $ \atom -> do
            void $ nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] fromLine' (from - fromOffset - 1) (-1)
            void $ nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] toLine' 0 (to - toOffset - 1)
            forM_ [fromLine' + 1 .. toLine' - 1] $ \line ->
              nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] line 0 (-1)
  | otherwise = pure ()
addHlBit _   _       (HlBit _     range) = nvim_err_writeln [i|Unexpected range format: #{range}|]

dispatchResponse :: Buffer -> Response -> Neovim AgdaEnv ()
dispatchResponse _   (Status (StatusInfo checked _ _))
  | checked = nvim_command "echo ''"
  | otherwise = pure ()
dispatchResponse _   (InteractionPoints x0) = pure ()
dispatchResponse _   (DisplayInfo di) = pure ()
dispatchResponse buf (HighlightingInfo (HlInfo _ bits)) = do
  ls <- nvim_buf_get_lines buf 0 (-1) False
  let offsets = scanl (\acc str -> acc + fromIntegral (BS.length str) + 1) 0 $ toList ls
  mapM_ (addHlBit buf offsets) bits
dispatchResponse _   (RunningInfo _ msg) = nvim_command [i|echom '#{msg}'|]
dispatchResponse buf ClearHighlighting = nvim_buf_clear_namespace buf (-1) 0 (-1)
dispatchResponse _   ClearRunningInfo = nvim_command "echo ''"

startAgda :: Neovim AgdaEnv ()
startAgda = do
  buf <- vim_get_current_buffer
  name <- BS.unpack <$> buffer_get_name buf

  agdasTVar <- asks agdas
  agdas <- readTVarIO agdasTVar
  unless (name `HM.member` agdas) $ do
    maybeInst <- startAgdaForFile () name
    case maybeInst of
         Nothing -> pure ()
         Just inst -> do
           watchErrors nvim_err_writeln inst
           watchStdout (parseResponse buf) inst
           loadFile inst

loadNecogda :: Neovim AgdaEnv ()
loadNecogda = startAgda

startStandalone :: FilePath -> ReaderT (AgdaEnvT ()) IO ()
startStandalone filename = do
  Just inst <- startAgdaForFile () filename
  watchErrors (liftIO . BS.putStrLn . ("[ERR] " <>)) inst
  watchStdout (liftIO . print . decodeResponse . stripMarker) inst
  loadFile inst
