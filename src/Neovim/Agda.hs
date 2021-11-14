{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Neovim.Agda
( defaultEnv
, loadNecogda
, initializePlugin
, necogdaComplete
) where

import Control.Monad
import Data.Char (toLower)
import Data.String.Interpolate.IsString
import Paths_necogda
import UnliftIO

import Neovim
import Neovim.API.ByteString (nvim_create_namespace, nvim_exec, nvim_err_writeln)

import Neovim.Agda.Input
import Neovim.Agda.Interaction
import Neovim.Agda.Start
import Neovim.Agda.Types
import Neovim.Agda.Util

registerMappings :: Neovim AgdaEnv ()
registerMappings = do
  forM_ goalCommands $ \(cmd, leader) -> do
    let str = [i|nnoremap <buffer><silent> <LocalLeader>#{leader}       :call NecogdaGoalCommand('#{cmd}', 'Simplified')<CR>|]
    void $ nvim_exec str False
    forM_ [minBound..maxBound :: Rewrite] $ \rewrite -> do
      let modifier = [toLower $ head $ show rewrite]
      nvim_exec [i|nnoremap <buffer><silent> <LocalLeader>#{modifier}#{leader}       :call NecogdaGoalCommand('#{cmd}', '#{rewrite}')<CR>|] False

  void $ nvim_exec [i|nnoremap <buffer><silent> <LocalLeader>r :call NecogdaRefine()<CR>|] False
  void $ nvim_exec [i|nnoremap <buffer><silent> <LocalLeader>c :call NecogdaMakeCase()<CR>|] False
  void $ nvim_exec [i|nnoremap <buffer><silent> <LocalLeader>a :call NecogdaAutoOne()<CR>|] False
  where
    goalCommands :: [(String, String)]
    goalCommands = [ ("TypeContextInfer", ".")
                   , ("TypeContext", ",")
                   , ("TypeContextCheck", ";")
                   , ("Type", "t")
                   ]

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

  res <- try $ do
    !trie <- liftIO $ loadInputTrie =<< getDataFileName "data/input/agda-emacs.txt"
    tvar <- asks symbolsTrie
    atomically $ writeTVar tvar trie
  case res of
       Right _ -> pure ()
       Left (ex :: SomeException) -> nvim_err_writeln [i|Unable to load input: #{ex}|]
