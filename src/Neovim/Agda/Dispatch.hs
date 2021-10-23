{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Neovim.Agda.Dispatch
( parseResponse
, dispatchResponse
) where

import qualified Data.ByteString.Char8 as BS
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

addHlBit :: Buffer -> [Int64] -> HlBit -> Neovim AgdaEnv ()
addHlBit buf offsets (HlBit atoms [from, to])
  | Just fromLine <- subtract 1 <$> findIndex (>= from) offsets
  , Just toLine <- subtract 1 <$> findIndex (>= to) offsets
    = let (fromOffset, toOffset) = join (***) (offsets !!) (fromLine, toLine)
          (fromLine', toLine') = join (***) fromIntegral (fromLine, toLine)
      in if fromLine' == toLine'
         then forM_ atoms $ \atom ->
            nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] fromLine' (from - fromOffset - 1) (to - toOffset - 1)
         else forM_ atoms $ \atom -> do
            void $ nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] fromLine' (from - fromOffset - 1) (-1)
            void $ nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] toLine' 0 (to - toOffset - 1)
            forM_ [fromLine' + 1 .. toLine' - 1] $ \line ->
              nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] line 0 (-1)
  | otherwise = pure ()
addHlBit _   _       (HlBit _     range) = nvim_err_writeln [i|Unexpected range format: #{range}|]

dispatchResponse :: Buffer -> Response -> Neovim AgdaEnv ()
dispatchResponse _   (Status (StatusInfo checked _ _))
  | checked = nvim_command "echo ''"
  | otherwise = pure ()
dispatchResponse _   (InteractionPoints pts) = pure ()
dispatchResponse _   (DisplayInfo di) = pure ()
dispatchResponse buf (HighlightingInfo (HlInfo _ bits)) = do
  ls <- nvim_buf_get_lines buf 0 (-1) False
  let offsets = scanl (\acc str -> acc + fromIntegral (BS.length str) + 1) 0 $ toList ls
  mapM_ (addHlBit buf offsets) bits
dispatchResponse _   (RunningInfo _ msg) = nvim_command [i|echom '#{msg}'|]
dispatchResponse buf ClearHighlighting = nvim_buf_clear_namespace buf (-1) 0 (-1)
dispatchResponse _   ClearRunningInfo = nvim_command "echo ''"
