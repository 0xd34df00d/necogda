module Main where

import Control.Concurrent
import Control.Monad.Reader
import System.Environment

import Neovim.Agda

main :: IO ()
main = do
  [filename] <- getArgs
  env <- defaultEnv
  void $ forkIO $ runReaderT (startStandalone filename) env
  void getLine
