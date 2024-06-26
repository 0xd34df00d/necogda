{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neovim.Agda
( defaultEnv
, loadNecogda
, initializePlugin
, necogdaComplete
) where

import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Char (toLower)
import Data.String.Interpolate.IsString
import Paths_necogda
import UnliftIO

import Neovim
import Neovim.API.ByteString (nvim_create_namespace, nvim_exec, nvim_err_writeln)

import Neovim.Agda.Interaction
import Neovim.Agda.Input
import Neovim.Agda.Nvim.VisualMarks
import Neovim.Agda.Start
import Neovim.Agda.Types

registerMappings :: Neovim AgdaEnv ()
registerMappings = forM_ goalCommands $ \(cmd, leader) -> do
  void $ nvim_exec [i|nnoremap <buffer><silent> <LocalLeader>#{leader}  :call NecogdaGoalCommand('#{cmd}', 'Simplified')<CR>|] False
  forM_ [minBound .. maxBound :: Rewrite] $ \rewrite -> do
    let modifier = map toLower $ take 1 $ show rewrite
    nvim_exec [i|nnoremap <buffer><silent> <LocalLeader>#{modifier}#{leader}  :call NecogdaGoalCommand('#{cmd}', '#{rewrite}')<CR>|] False
  where
    goalCommands :: [(String, String)]
    goalCommands = [ ("TypeContextInfer", ".")
                   , ("TypeContext", ",")
                   , ("TypeContextCheck", ";")
                   , ("Type", "t")
                   ]

registerSigns :: Neovim env ()
registerSigns = forM_ [minBound .. maxBound] $ \kind ->
  nvim_exec [i|call sign_define("#{kindSignName kind}", { "text": "#{kindSymbol kind}", "texthl": "#{kindHighlightName kind}" })|] False

loadNecogda :: Neovim AgdaEnv ()
loadNecogda = do
  startAgda
  startInput
  registerMappings
  registerSigns

initNamespace :: BS.ByteString -> (AgdaEnv -> TVar Int64) -> Neovim AgdaEnv ()
initNamespace name tvar = do
  nsId <- nvim_create_namespace name
  nsTvar <- asks tvar
  atomically $ writeTVar nsTvar nsId

initializePlugin :: Neovim AgdaEnv ()
initializePlugin = do
  initNamespace "necogda_goalmarks" goalmarksNs
  initNamespace "necogda_highlight" highlightNs
  initNamespace "necogda_virtualmarks" virtualMarksNs

  res <- try $ do
    !trie <- liftIO $ loadInputTrie =<< getDataFileName "data/input/agda-emacs.txt"
    tvar <- asks symbolsTrie
    atomically $ writeTVar tvar trie
  case res of
       Right _ -> pure ()
       Left (ex :: SomeException) -> nvim_err_writeln [i|Unable to load input: #{ex}|]
