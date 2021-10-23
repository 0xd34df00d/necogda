{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Neovim.Agda.Input(startInput) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Trie as Trie
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad
import Data.Maybe
import UnliftIO

import Neovim
import Neovim.API.ByteString

import Neovim.Agda.Types

sampleTrie :: Trie.Trie T.Text
sampleTrie = Trie.fromList [ ("all", "∀")
                           , ("forall", "∀")
                           , ("Ga", "α")
                           , ("Gb", "β")
                           ]

data Cursor = Cursor { row :: Int, col :: Int }
  deriving (Eq, Ord, Show)

getCurColI :: Neovim env Cursor
getCurColI = do
  (r, c) <- nvim_win_get_cursor =<< nvim_get_current_win
  pure $ Cursor (fromIntegral r) (fromIntegral c - 1)
  -- -1 because the cursor points to the next symbol after insertion

handleInput :: Neovim AgdaEnv ()
handleInput = do
  maybeStartCol <- asks symbolInputCol >>= readTVarIO
  case maybeStartCol of
       Nothing -> maybeStartSymbol
       Just start -> handleNextSymbol start

maybeStartSymbol :: Neovim AgdaEnv ()
maybeStartSymbol = do
  line <- nvim_get_current_line
  unless (BS.null line) $ do
    curCol <- col <$> getCurColI
    case line `BS.index` curCol of
         '`' -> asks symbolInputCol >>= \var -> atomically $ writeTVar var (Just curCol)
         _   -> pure ()

split3 :: BS.ByteString -> Int -> Int -> (BS.ByteString, BS.ByteString, BS.ByteString)
split3 str start len = (left, mid, right)
  where
    (left, rest) = BS.splitAt start str
    (mid, right) = BS.splitAt len rest

handleNextSymbol :: Int -> Neovim AgdaEnv ()
handleNextSymbol start = do
  line <- nvim_get_current_line
  Cursor { .. } <- getCurColI
  if col >= start
  then do
    let (left, mid, right) = split3 line start (col - start + 1)
    case Trie.lookup (BS.tail mid) sampleTrie of
         Nothing  -> pure ()
         Just sub -> do
           let subBytes = T.encodeUtf8 sub
           nvim_set_current_line $ left <> subBytes <> right
           win <- nvim_get_current_win
           nvim_win_set_cursor win (fromIntegral row, fromIntegral $ start + BS.length subBytes)
           cancelInput
  else cancelInput

cancelInput :: Neovim AgdaEnv ()
cancelInput = asks symbolInputCol >>= \var -> atomically $ writeTVar var Nothing

startInput :: Neovim AgdaEnv ()
startInput = forM_ commands $ \(event, cmd) -> do
  res <- addAutocmd event Sync (AutocmdOptions "<buffer>" False Nothing) cmd
  when (isNothing res) $ nvim_err_writeln "Unable to register input handler"
  where
    commands = ((, cancelInput) <$> ["InsertLeave"])
            <> ((, handleInput) <$> ["TextChangedI", "TextChangedP"])
