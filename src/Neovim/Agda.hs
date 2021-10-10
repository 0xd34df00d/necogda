module Neovim.Agda
( defaultEnv
, loadNecogda
) where

import Neovim

data AgdaEnv = AgdaEnv
  {
  }
  deriving (Eq, Show)

defaultEnv :: AgdaEnv
defaultEnv = AgdaEnv

loadNecogda :: Neovim AgdaEnv ()
loadNecogda = pure ()
