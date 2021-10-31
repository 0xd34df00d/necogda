{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Neovim.Agda.Commands
( sendCommand

, loadFile
, goalCommand
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

goalCommandCtor :: String -> Maybe (Rewrite -> InteractionId -> Range -> String -> Interaction)
goalCommandCtor "Context"          = Just Cmd_context
goalCommandCtor "Type"             = Just Cmd_goal_type
goalCommandCtor "TypeContext"      = Just Cmd_goal_type_context
goalCommandCtor "TypeContextInfer" = Just Cmd_goal_type_context_infer
goalCommandCtor _ = Nothing

goalCommand :: String -> Rewrite -> Neovim AgdaEnv ()
goalCommand cmd rewrite = withInstance $ \agda -> do
  maybeInteractionId <- getCurrentInteractionId agda
  case maybeInteractionId of
       Nothing -> nvim_err_writeln [i|Not in an interaction point|]
       Just iid -> case goalCommandCtor cmd of
                        Nothing -> nvim_err_writeln [i|Unknown goal command: #{cmd}|]
                        Just ctor -> sendCommand agda $ ctor rewrite iid NoRange ""
