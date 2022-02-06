{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Neovim.Agda.Input
( startInput
, necogdaComplete

, loadInputTrie
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Trie as Trie
import qualified Data.Trie.Convenience as Trie
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
import Neovim.Agda.Util

getMarker :: Neovim env Char
getMarker = do
  -- TODO cache this
  markerObj <- nvim_eval [i|get(g:, 'symbol_start_marker', '`')|]
  case markerObj of
       ObjectString (BS.unpack -> [c]) -> pure c
       _ -> do nvim_err_writeln "Invalid symbol start marker"
               pure '`'

loadInputTrie :: FilePath -> IO InputTrie
loadInputTrie path = parseContents <$> BS.readFile path
  where
    parseContents contents = Trie.fromListWith (<>)
                             [ (abbrev, T.decodeUtf8 <$> codes)
                             | l <- BS.lines contents
                             , let (abbrev : codes) = BS.split ' ' l
                             ]

getInputTrie :: Neovim AgdaEnv InputTrie
getInputTrie = asks symbolsTrie >>= readTVarIO
  where
    sampleTrie :: InputTrie
    sampleTrie = Trie.fromList [ ("all", ["∀"])
                               , ("alls", ["∀"])
                               , ("forall", ["∀"])
                               , ("Ga", ["α"])
                               , ("Gb", ["β"])
                               ]
    _unused = sampleTrie

getCursorI :: Neovim env Cursor
getCursorI = do
  (r, c) <- nvim_win_get_cursor =<< nvim_get_current_win
  pure $ Cursor (fromIntegral r) (fromIntegral c)

handleInput :: Neovim AgdaEnv ()
handleInput = do
  maybeStartCol <- asks symbolInputCol >>= readTVarIO
  maybe maybeStartSymbol handleNextSymbol maybeStartCol

maybeStartSymbol :: Neovim AgdaEnv ()
maybeStartSymbol = do
  line <- nvim_get_current_line
  curCol <- col <$> getCursorI
  unless (curCol == 0) $ do
    marker <- getMarker
    when (line `BS.index` (curCol - 1) == marker) $ setStartSymbol (curCol - 1)

setStartSymbol :: Int -> Neovim AgdaEnv ()
setStartSymbol curCol = asks symbolInputCol >>= \var -> atomically $ writeTVar var (Just curCol)

split3 :: BS.ByteString -> Int -> Int -> (BS.ByteString, BS.ByteString, BS.ByteString)
split3 str start len = (left, mid, right)
  where
    (left, rest) = BS.splitAt start str
    (mid, right) = BS.splitAt len rest

handleSubstr :: Char -> Int -> Cursor -> BS.ByteString -> Neovim AgdaEnv ()
handleSubstr marker start Cursor { .. } line
  | BS.null mid = pure ()
  | otherwise = getInputTrie >>= Trie.lookupBy handlePrefixResult (BS.init mid)
  where
    (left, BS.tail -> mid, right) = split3 line start (col - start)

    handlePrefixResult prefixNode = uncurry (handleFullResult prefixNode) . Trie.lookupBy (,) (BS.singleton $ BS.last mid)

    handleFullResult :: Maybe [T.Text]  -- prefixNode
                     -> Maybe [T.Text]  -- fullNode
                     -> InputTrie       -- fullChidren
                     -> Neovim AgdaEnv ()
    handleFullResult _ (Just subs) children
      | Trie.null children = insertBytes $ T.encodeUtf8 $ head subs
      | otherwise = pure ()
    handleFullResult (Just subs) Nothing children
      | Trie.null children = do insertBytes $ T.encodeUtf8 (head subs) `BS.snoc` BS.last mid
                                when (BS.last mid == marker) maybeStartSymbol
      | otherwise = pure ()
    handleFullResult Nothing Nothing children
      | Trie.null children = cancelInput
      | otherwise = pure ()

    insertBytes bytes = do
      insertBehaviour <- nvim_eval [i|get(g:, 'insert_behaviour', '')|]
      case insertBehaviour of
           ObjectString "undo" -> do nvim_set_current_line $ left <> bytes <> right
                                     win <- nvim_get_current_win
                                     nvim_win_set_cursor win (fromIntegral row, fromIntegral $ start + BS.length bytes)
           _                   -> void $ nvim_input [i|<ESC>v#{col - start - 1}hs#{bytes}|]
      cancelInput

handleNextSymbol :: Int -> Neovim AgdaEnv ()
handleNextSymbol start = do
  cur@Cursor { .. } <- getCursorI
  marker <- getMarker
  if col <= start
  then cancelInput
  else nvim_get_current_line >>= handleSubstr marker start cur

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
necogdaComplete 1 = wrapExceptions . const checkCompletionStart
necogdaComplete 0 = wrapExceptions . generateCompletions
necogdaComplete m = const $ nvim_err_writeln [i|Invalid complete mode: #{m}|] $> ObjectNil

wrapExceptions :: Neovim AgdaEnv Object -> Neovim AgdaEnv Object
wrapExceptions = try >=> either showEx pure
  where
    showEx :: SomeException -> Neovim AgdaEnv Object
    showEx ex = nvim_err_writeln [i|Error running completion: #{ex}|] >> dump (show ex) $> ObjectNil

checkCompletionStart :: Neovim AgdaEnv Object
checkCompletionStart = do
  line <- nvim_get_current_line
  col <- col <$> getCursorI
  marker <- getMarker
  pure $ case marker `BS.elemIndexEnd` BS.take col line of
              Just idx -> ObjectInt $ fromIntegral idx
              Nothing  -> ObjectInt (-3)

generateCompletions :: BS.ByteString -> Neovim AgdaEnv Object
generateCompletions base = do
  (maybeVal, children) <- Trie.lookupBy (,) (BS.tail base) <$> getInputTrie
  let childrenOpts = map (first (base <>)) $ take 15 $ sortOn (BS.length . fst) $ Trie.toList children
  let opts = case maybeVal of
                  Just val -> (base, val) : childrenOpts
                  Nothing -> childrenOpts
  let vimOpts = [ M.fromList [ (ObjectString "word", ObjectString $ T.encodeUtf8 sym)
                             , (ObjectString "equal", ObjectInt 1)
                             , (ObjectString "menu", ObjectString [i|(#{abbr})|])
                             ]
                | (abbr, syms) <- opts
                , sym <- syms
                ]
  void $ nvim_eval [i|timer_start(0, { _ -> pumvisible() ? nvim_input("<down>") : nvim_input("") })|]
  pure $ ObjectMap $ M.fromList [ (ObjectString "words", ObjectArray $ ObjectMap <$> vimOpts)
                                , (ObjectString "refresh", ObjectString "always")
                                ]
