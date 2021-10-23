{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Neovim.Agda.Input(startInput, necogdaComplete) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Trie as Trie
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Arrow (first)
import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe
import Data.String.Interpolate.IsString
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

getCursorI :: Neovim env Cursor
getCursorI = do
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
    curCol <- col <$> getCursorI
    case line `BS.index` curCol of
         '`' -> startSymbol curCol
         _   -> pure ()

startSymbol :: Int -> Neovim AgdaEnv ()
startSymbol curCol = asks symbolInputCol >>= \var -> atomically $ writeTVar var (Just curCol)

split3 :: BS.ByteString -> Int -> Int -> (BS.ByteString, BS.ByteString, BS.ByteString)
split3 str start len = (left, mid, right)
  where
    (left, rest) = BS.splitAt start str
    (mid, right) = BS.splitAt len rest

handleSubstr :: Int -> Cursor -> BS.ByteString -> Neovim AgdaEnv ()
handleSubstr start Cursor { .. } line = Trie.lookupBy handleTrieResult (if isComplete then BS.init mid else mid) sampleTrie
  where
    (left, BS.tail -> mid, right) = split3 line start (col - start + 1)
    isComplete = not (BS.null mid) && BS.last mid `elem` ['`', ' ']

    handleTrieResult Nothing children
      | Trie.null children = cancelInput
      | otherwise = pure ()
    handleTrieResult (Just sub) children
      | Trie.null children = insertBytes $ T.encodeUtf8 sub
      | isComplete = insertBytes $ T.encodeUtf8 sub `BS.snoc` BS.last mid
      | otherwise = pure ()

    insertBytes bytes = do
      insertBehaviour <- nvim_eval [i|get(g:, 'insert_behaviour', '')|]
      case insertBehaviour of
           ObjectString "undo" -> do nvim_set_current_line $ left <> bytes <> right
                                     win <- nvim_get_current_win
                                     nvim_win_set_cursor win (fromIntegral row, fromIntegral $ start + BS.length bytes)
           _                   -> void $ nvim_input [i|<ESC>v#{col - start}hdi#{bytes}|]
      cancelInput

handleNextSymbol :: Int -> Neovim AgdaEnv ()
handleNextSymbol start = do
  cur@Cursor { .. } <- getCursorI
  if col < start
  then cancelInput
  else nvim_get_current_line >>= handleSubstr start cur

cancelInput :: Neovim AgdaEnv ()
cancelInput = asks symbolInputCol >>= \var -> atomically $ writeTVar var Nothing

startInput :: Neovim AgdaEnv ()
startInput = forM_ commands $ \(event, cmd) -> do
  res <- addAutocmd event Sync (AutocmdOptions "<buffer>" False Nothing) cmd
  when (isNothing res) $ nvim_err_writeln "Unable to register input handler"
  where
    commands = ((, cancelInput) <$> ["InsertLeave"])
            <> ((, handleInput) <$> ["TextChangedI", "TextChangedP"])

necogdaComplete :: Int -> BS.ByteString -> Neovim AgdaEnv Object
necogdaComplete 1 _ = do
  line <- nvim_get_current_line
  col <- col <$> getCursorI
  pure $ case BS.elemIndexEnd '`' $ BS.take (col + 1) line of
              Just idx -> ObjectInt $ fromIntegral idx
              Nothing  -> ObjectInt (-3)
necogdaComplete 0 base = do
  let (maybeVal, children) = Trie.lookupBy (,) (BS.tail base) sampleTrie
  let childrenOpts = map (first (base <>)) $ take 15 $ sortOn (BS.length . fst) $ Trie.toList children
  let opts = case maybeVal of
                  Just val -> (base, val) : childrenOpts
                  Nothing -> childrenOpts
  let vimOpts = [ M.fromList [ (ObjectString "word", ObjectString $ T.encodeUtf8 sym)
                             , (ObjectString "equal", ObjectInt 1)
                             , (ObjectString "menu", ObjectString [i|(#{abbr})|])
                             ]
                | (abbr, sym) <- opts
                ]
  pure $ ObjectMap $ M.fromList [ (ObjectString "words", ObjectArray $ ObjectMap <$> vimOpts)
                                , (ObjectString "refresh", ObjectString "always")
                                ]
necogdaComplete mode _ = nvim_err_writeln [i|Invalid complete mode: #{mode}|] $> ObjectNil