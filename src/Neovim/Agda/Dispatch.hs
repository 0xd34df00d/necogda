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
import qualified Data.Vector as V
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

data DispatchContext = DispatchContext
  { agdaBuffer :: Buffer
  , outputBuffer :: Buffer
  }

dispatchResponse :: DispatchContext -> Response -> Neovim AgdaEnv ()
dispatchResponse _   (Status (StatusInfo checked _ _))
  | checked = nvim_command "echo ''"
  | otherwise = pure ()
dispatchResponse ctx (InteractionPoints pts) = pure ()
dispatchResponse ctx (DisplayInfo AllGoalsWarnings { .. }) =
  setOutputBuffer ctx $ fmtGoals "Goals" (T.pack . show . id'range) visibleGoals
                     <> fmtGoals "Invisible" name'range invisibleGoals
  where
    fmtGoals :: String -> (range -> T.Text) -> [Goal range] -> V.Vector T.Text
    fmtGoals _    _        [] = V.empty
    fmtGoals name fmtRange goals = V.fromList ([i|#{name}:|] : (fmtGoal <$> goals)) <> V.singleton " "
      where
        fmtGoal OfType {..} = [i|?#{fmtRange constraintObj}: #{type'goal}|] :: T.Text
        fmtGoal JustSort { .. } = [i|?#{fmtRange constraintObj}: Sort|]
dispatchResponse _   (DisplayInfo Version {}) = pure ()
dispatchResponse ctx (DisplayInfo Error { .. }) = setOutputBuffer ctx [message'error error']
dispatchResponse ctx (DisplayInfo GoalSpecific { goalInfo = GoalInfo { .. } }) = setOutputBuffer ctx $ V.fromList $ header : (fmtEntry <$> entries)
  where
    header = "Goal: " <> type'goal <> "\n" <> T.replicate 40 "-"
    fmtEntry GoalContextEntry { .. } = let scopeMarker = if inScope then T.empty else " (not in scope)"
                                           reifyMarker = if originalName == reifiedName then "" else " (renamed to " <> reifiedName <> ")"
                                        in [i|#{originalName}#{reifyMarker}: #{binding}#{scopeMarker}|]
dispatchResponse ctx (HighlightingInfo (HlInfo _ bits)) = do
  ls <- toList . fmap T.decodeUtf8 <$> nvim_buf_get_lines (agdaBuffer ctx) 0 (-1) False
  let offsets = scanl (\acc str -> acc + fromIntegral (T.length str) + 1) 0 $ toList ls
  mapM_ (addHlBit (agdaBuffer ctx) offsets ls) bits
dispatchResponse _   (RunningInfo _ msg) = nvim_command [i|echom '#{msg}'|]
dispatchResponse ctx ClearHighlighting = nvim_buf_clear_namespace (agdaBuffer ctx) (-1) 0 (-1)
dispatchResponse _   ClearRunningInfo = nvim_command "echo ''"
dispatchResponse _   JumpToError { .. } = pure ()

codepoint2byte :: T.Text -> Int64 -> Int64
codepoint2byte line cp = fromIntegral $ BS.length $ T.encodeUtf8 $ T.take (fromIntegral cp) line

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

setOutputBuffer :: Foldable f => DispatchContext -> f T.Text -> Neovim env ()
setOutputBuffer ctx = nvim_buf_set_lines (outputBuffer ctx) 0 (-1) False
                    . V.concatMap (V.fromList . BS.split '\n' . T.encodeUtf8)
                    . V.fromList
                    . toList
