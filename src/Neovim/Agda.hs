{-# LANGUAGE OverloadedStrings #-}

module Neovim.Agda
( defaultEnv
, loadNecogda
, initializePlugin
, necogdaComplete
) where

import Neovim
import Neovim.API.ByteString (nvim_create_namespace)

import Neovim.Agda.Input
import Neovim.Agda.Start
import Neovim.Agda.Types
import UnliftIO

loadNecogda :: Neovim AgdaEnv ()
loadNecogda = do
  startAgda
  startInput

initializePlugin :: Neovim AgdaEnv ()
initializePlugin = do
  goalmarksId <- nvim_create_namespace "necogda_goalmarks"
  goalmarksTVar <- asks goalmarksNs
  atomically $ writeTVar goalmarksTVar goalmarksId
