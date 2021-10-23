{-# LANGUAGE TemplateHaskell #-}

module Main where

import Neovim
import qualified Neovim.Agda as P

main :: IO ()
main = do
  env <- P.defaultEnv
  let plugin = wrapPlugin Plugin
                  { environment = env
                  , exports = [ $(function' 'P.loadNecogda) Async
                              , $(function' 'P.necogdaComplete) Sync
                              ]
                  }
  neovim defaultConfig { plugins = [plugin], logOptions = Just ("nvim.log", DEBUG) }
