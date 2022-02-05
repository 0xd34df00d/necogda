{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Control.Concurrent
import Control.Monad.Reader
import System.Environment

import Neovim.Agda

import Neovim.Agda.Request.Commands (sendCommand)
import Neovim.Agda.Response.Dispatch
import Neovim.Agda.Interaction
import Neovim.Agda.Start
import Neovim.Agda.Types

startStandalone :: FilePath -> ReaderT (AgdaEnvT ()) IO ()
startStandalone file = do
  Just inst <- startAgdaForFile () file
  watchErrors errHandler inst
  watchStdout (liftIO . print . parseResponse) errHandler inst
  sendCommand inst $ Cmd_load file []
  where
    errHandler = liftIO . BS.putStrLn . ("[ERR] " <>)

main :: IO ()
main = do
  [file] <- getArgs
  env <- defaultEnv
  void $ forkIO $ runReaderT (startStandalone file) env
  void getLine
