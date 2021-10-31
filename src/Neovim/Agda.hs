{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neovim.Agda
( defaultEnv
, loadNecogda
, initializePlugin
, necogdaComplete
) where

import Control.Monad
import Data.Char (toLower)
import Data.String.Interpolate.IsString
import UnliftIO

import Neovim
import Neovim.API.ByteString (nvim_create_namespace, nvim_exec)

import Neovim.Agda.Input
import Neovim.Agda.Interaction
import Neovim.Agda.Start
import Neovim.Agda.Types

registerMappings :: Neovim AgdaEnv ()
registerMappings = do
  forM_ [("TypeContextInfer", "."), ("Type", "t"), ("Context", "e")] $ \(cmd :: String, leader :: String) -> do
    let str = [i|nnoremap <buffer><silent> <LocalLeader>#{leader}       :call NecogdaGoalCommand('#{cmd}', 'Simplified')<CR>|]
    void $ nvim_exec str False
    forM_ [minBound..maxBound :: Rewrite] $ \rewrite -> do
      let modifier = [toLower $ head $ show rewrite]
      nvim_exec [i|nnoremap <buffer><silent> <LocalLeader>#{modifier}#{leader}       :call NecogdaGoalCommand('#{cmd}', '#{rewrite}')<CR>|] False

loadNecogda :: Neovim AgdaEnv ()
loadNecogda = do
  startAgda
  startInput
  registerMappings

initializePlugin :: Neovim AgdaEnv ()
initializePlugin = do
  goalmarksId <- nvim_create_namespace "necogda_goalmarks"
  goalmarksTVar <- asks goalmarksNs
  atomically $ writeTVar goalmarksTVar goalmarksId
