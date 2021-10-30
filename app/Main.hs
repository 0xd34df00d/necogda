{-# LANGUAGE TemplateHaskell #-}

module Main where

import Neovim

import qualified Neovim.Agda as P
import qualified Neovim.Agda.Commands as C

main :: IO ()
main = do
  env <- P.defaultEnv
  let plugin = wrapPlugin Plugin
                  { environment = env
                  , exports = [ $(function' 'P.initializePlugin) Async
                              , $(function' 'P.loadNecogda) Async

                              , $(function' 'P.necogdaComplete) Sync

                              , $(function "NecogdaLoadFile" 'C.loadFile) Async
                              , $(function "NecogdaGoalInfo" 'C.goalInfo) Async
                              ]
                  }
  neovim defaultConfig { plugins = [plugin], logOptions = Just ("nvim.log", DEBUG) }
