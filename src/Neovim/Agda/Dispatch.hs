{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Neovim.Agda.Dispatch
( parseResponse
, dispatchResponse
, DispatchContext(..)
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.List
import Data.String.Interpolate.IsString

import Neovim
import Neovim.API.ByteString

import Neovim.Agda.Response
import Neovim.Agda.Types

stripMarker :: BS.ByteString -> BS.ByteString
stripMarker str
  | marker `BS.isPrefixOf` str = BS.drop (BS.length marker) str
  | otherwise = str
  where
    marker = "JSON> "

parseResponse :: BS.ByteString -> Either String Response
parseResponse = decodeResponse . stripMarker

codepoint2byte :: T.Text -> Int64 -> Int64
codepoint2byte line cp = fromIntegral $ BS.length $ T.encodeUtf8 $ T.take (fromIntegral cp) line

data DispatchContext = DispatchContext
  { agdaBuffer :: Buffer
  , outputBuffer :: Buffer
  }

addHlBit :: Buffer -> [Int64] -> [T.Text] -> HlBit -> Neovim AgdaEnv ()
addHlBit buf offsets ls (HlBit atoms [from, to])
  | Just fromLine <- subtract 1 <$> findIndex (>= from) offsets
  , Just toLine <- subtract 1 <$> findIndex (>= to) offsets
    = let (fromOffset, toOffset) = join (***) (offsets !!) (fromLine, toLine)
          (fromLine', toLine') = join (***) fromIntegral (fromLine, toLine)
          (fromStr, toStr) = join (***) (ls !!) (fromLine, toLine)
      in if fromLine' == toLine'
         then forM_ atoms $ \atom ->
            nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] fromLine' (codepoint2byte fromStr $ from - fromOffset - 1) (codepoint2byte toStr $ to - toOffset - 1)
         else forM_ atoms $ \atom -> do
            void $ nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] fromLine' (codepoint2byte fromStr $ from - fromOffset - 1) (-1)
            void $ nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] toLine' 0 (codepoint2byte toStr   $ to - toOffset - 1)
            forM_ [fromLine' + 1 .. toLine' - 1] $ \line ->
              nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] line 0 (-1)
  | otherwise = pure ()
addHlBit _   _       _  (HlBit _     range) = nvim_err_writeln [i|Unexpected range format: #{range}|]

dispatchResponse :: Buffer -> Response -> Neovim AgdaEnv ()
dispatchResponse _   (Status (StatusInfo checked _ _))
  | checked = nvim_command "echo ''"
  | otherwise = pure ()
dispatchResponse _   (InteractionPoints pts) = pure ()
dispatchResponse _   (DisplayInfo di) = pure ()
dispatchResponse ctx (HighlightingInfo (HlInfo _ bits)) = do
  ls <- toList . fmap T.decodeUtf8 <$> nvim_buf_get_lines (agdaBuffer ctx) 0 (-1) False
  let offsets = scanl (\acc str -> acc + fromIntegral (T.length str) + 1) 0 $ toList ls
  mapM_ (addHlBit (agdaBuffer ctx) offsets ls) bits
dispatchResponse _   (RunningInfo _ msg) = nvim_command [i|echom '#{msg}'|]
dispatchResponse ctx ClearHighlighting = nvim_buf_clear_namespace (agdaBuffer ctx) (-1) 0 (-1)
dispatchResponse _   ClearRunningInfo = nvim_command "echo ''"
