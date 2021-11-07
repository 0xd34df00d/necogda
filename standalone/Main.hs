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
startStandalone filename = do
  Just inst <- startAgdaForFile () filename
  watchErrors (liftIO . BS.putStrLn . ("[ERR] " <>)) inst
  watchStdout (liftIO . print . parseResponse) inst
  sendCommand inst $ Cmd_load filename []

main :: IO ()
main = do
  [filename] <- getArgs
  env <- defaultEnv
  void $ forkIO $ runReaderT (startStandalone filename) env
  void getLine
