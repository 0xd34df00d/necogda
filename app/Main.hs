{-# LANGUAGE TemplateHaskell #-}

module Main where

import Neovim
import qualified Neovim.Agda as P

plugin :: Neovim () NeovimPlugin
plugin = do
  env <- P.defaultEnv
  wrapPlugin Plugin
    { environment = env
    , exports = [ $(function' 'P.loadNecogda) Async ]
    }

main :: IO ()
main = neovim defaultConfig { plugins = [plugin], logOptions = Just ("nvim.log", DEBUG) }
