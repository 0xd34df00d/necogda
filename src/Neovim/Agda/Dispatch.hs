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
import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe
import Data.String.Interpolate.IsString

import Neovim
import Neovim.API.ByteString

import Neovim.Agda.Interaction
import Neovim.Agda.Response
import Neovim.Agda.Types
import Neovim.Agda.Util

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

  , withPayload :: (NeovimPayload -> Neovim AgdaEnv ()) -> Neovim AgdaEnv ()
  , modifyPayload :: (NeovimPayload -> NeovimPayload) -> Neovim AgdaEnv ()
  }

fmtGoalContextEntry :: GoalContextEntry -> T.Text
fmtGoalContextEntry GoalContextEntry { .. } = [i|#{originalName}#{reifyMarker}: #{binding}#{scopeMarker}|]
  where
    scopeMarker = if inScope then T.empty else " (not in scope)"
    reifyMarker = if originalName == reifiedName then "" else " (renamed to " <> reifiedName <> ")"

dispatchGoalInfo :: DispatchContext -> GoalInfo -> Neovim AgdaEnv ()
dispatchGoalInfo ctx GoalType { .. } = setOutputBuffer ctx $ V.fromList $ header : (fmtGoalContextEntry <$> entries)
  where
    header = "Goal: " <> type'goal <> "\n" <> T.replicate 40 "-"
dispatchGoalInfo ctx CurrentGoal { .. } = setOutputBuffer ctx ["Goal: " <> type'goal]

dispatchResponse :: DispatchContext -> Response -> Neovim AgdaEnv ()
dispatchResponse _   (Status (StatusInfo checked _ _))
  | checked = nvim_command "echo ''"
  | otherwise = pure ()
dispatchResponse ctx (InteractionPoints pts) = do
  marks <- setInteractionMarks (agdaBuffer ctx) pts
  modifyPayload ctx $ \p -> p { markId2interactionPoint = marks }
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
dispatchResponse ctx (DisplayInfo GoalSpecific { .. }) = dispatchGoalInfo ctx goalInfo
dispatchResponse ctx (HighlightingInfo (HlInfo _ bits)) = do
  p2c <- preparePosition2Cursor $ agdaBuffer ctx
  mapM_ (addHlBit (agdaBuffer ctx) p2c) bits
dispatchResponse _   (RunningInfo _ msg) = nvim_command [i|echom '#{msg}'|]
dispatchResponse ctx ClearHighlighting = nvim_buf_clear_namespace (agdaBuffer ctx) (-1) 0 (-1)
dispatchResponse _   ClearRunningInfo = nvim_command "echo ''"
dispatchResponse _   JumpToError { .. } = pure ()


addHlBit :: Buffer -> Position2Cursor -> HlBit -> Neovim AgdaEnv ()
addHlBit buf pos2cur (HlBit atoms [fromPos, toPos])
  | Just from <- position2cursor pos2cur fromPos
  , Just to <- position2cursor pos2cur toPos = mapM_ (onRange (uncurry Cursor from) (uncurry Cursor to) . highlight) atoms
  | otherwise = pure ()
  where
    highlight atom row fromCol toCol = void $ nvim_buf_add_highlight buf (-1) [i|agda_atom_#{atom}|] row (fromMaybe 0 fromCol) (fromMaybe (-1) toCol)
addHlBit _   _       (HlBit _     range) = nvim_err_writeln [i|Unexpected range format: #{range}|]


data Position2Cursor = Position2Cursor
  { linesOffsets :: [Int64]
  , linesContents :: [T.Text]
  }

preparePosition2Cursor :: Buffer -> Neovim AgdaEnv Position2Cursor
preparePosition2Cursor buf = do
  linesContents <- toList . fmap T.decodeUtf8 <$> nvim_buf_get_lines buf 0 (-1) False
  let linesOffsets = scanl (\acc str -> acc + fromIntegral (T.length str) + 1) 0 $ toList linesContents
  pure Position2Cursor { .. }

position2cursor :: Position2Cursor -> Int64 -> Maybe (Int64, Int64)
position2cursor Position2Cursor { .. } pos = do
  lineIdx <- subtract 1 <$> findIndex (>= pos) linesOffsets
  let lineOffset = linesOffsets !! lineIdx
      colIdx = codepoint2byte (linesContents !! lineIdx) (pos - lineOffset - 1)
  pure (fromIntegral lineIdx, colIdx)

codepoint2byte :: T.Text -> Int64 -> Int64
codepoint2byte line cp = fromIntegral $ BS.length $ T.encodeUtf8 $ T.take (fromIntegral cp) line


setOutputBuffer :: Foldable f => DispatchContext -> f T.Text -> Neovim env ()
setOutputBuffer ctx = nvim_buf_set_lines (outputBuffer ctx) 0 (-1) False
                    . V.concatMap (V.fromList . BS.split '\n' . T.encodeUtf8)
                    . V.fromList
                    . toList
