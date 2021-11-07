{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Neovim.Agda.Commands
( sendCommand

, loadFile
, goalCommand
, refine
, makeCase
, autoOne
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
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

withInteractionId :: (AgdaInstance -> InteractionId -> T.Text -> Neovim AgdaEnv ()) -> Neovim AgdaEnv ()
withInteractionId f = withInstance $ \agda -> do
  maybeInteractionId <- getCurrentInteractionId agda
  case maybeInteractionId of
       Nothing -> nvim_err_writeln [i|Not in an interaction point|]
       Just (iid, text) -> f agda iid text

goalCommand :: String -> Rewrite -> Neovim AgdaEnv ()
goalCommand cmd rewrite = case lookup cmd ctors of
                               Just ctor -> withInteractionId $ \agda iid text -> sendCommand agda $ ctor rewrite iid NoRange (T.unpack text)
                               Nothing -> nvim_err_writeln [i|Unknown goal command: #{cmd}|]
  where
    ctors = [ ("Type", Cmd_goal_type)
            , ("TypeContext", Cmd_goal_type_context)
            , ("TypeContextInfer", Cmd_goal_type_context_infer)
            , ("TypeContextCheck", Cmd_goal_type_context_check)
            ]

refine :: Neovim AgdaEnv ()
refine = withInteractionId $ \agda iid text -> sendCommand agda $ Cmd_refine_or_intro False iid NoRange (T.unpack text)

makeCase :: Neovim AgdaEnv ()
makeCase = withInteractionId $ \agda iid text -> sendCommand agda $ Cmd_make_case iid NoRange (T.unpack text)

autoOne :: Neovim AgdaEnv ()
autoOne = withInteractionId $ \agda iid text -> sendCommand agda $ Cmd_autoOne iid NoRange (T.unpack text)
