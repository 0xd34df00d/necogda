{-# LANGUAGE TemplateHaskell #-}

module Main where

import Neovim
import qualified Neovim.Agda as P

plugin :: Neovim () NeovimPlugin
plugin = wrapPlugin Plugin
  { environment = P.defaultEnv
  , exports = [ $(function' 'P.loadNecogda) Sync ]
  }

main :: IO ()
main = neovim defaultConfig { plugins = [plugin] }
