module Neovim.Agda
( defaultEnv
, loadNecogda
, necogdaComplete

, startStandalone
) where

import Neovim

import Neovim.Agda.Input
import Neovim.Agda.Start
import Neovim.Agda.Types

loadNecogda :: Neovim AgdaEnv ()
loadNecogda = do
  startAgda
  startInput
