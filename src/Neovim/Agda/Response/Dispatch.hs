{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}

module Neovim.Agda.Response.Dispatch
( parseResponse
, dispatchResponse
, DispatchContext(..)
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Identity
import Data.Default
import Data.Foldable
import Data.List
import Data.Maybe
import Data.String.Interpolate.IsString
import UnliftIO

import Neovim
import Neovim.API.ByteString

import Neovim.Agda.Interaction
import Neovim.Agda.Response.Types as R
import Neovim.Agda.Types
import Neovim.Agda.Util as U

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

  , withPayload :: forall a. Default a => (NeovimPayload -> Neovim AgdaEnv a) -> Neovim AgdaEnv a
  , modifyPayload :: (NeovimPayload -> NeovimPayload) -> Neovim AgdaEnv ()
  }

addMarks :: DispatchContext -> (range -> [R.Range]) -> [Goal range] -> Neovim AgdaEnv ()
addMarks ctx getRange = mapM_ $ \goal -> setVirtualText (getRange $ constraintObj goal) (fmtGoalType goal)
  where
    buf = agdaBuffer ctx

    setVirtualText :: [R.Range] -> T.Text -> Neovim AgdaEnv ()
    setVirtualText [R.Range { .. }] text = do
      hlId <- asks highlightNs >>= readTVarIO
      void $ nvim_buf_set_extmark buf hlId (R.line start - 1) (R.col start) [ ("end_line", ObjectInt $ R.line end - 1)
                                                                            , ("end_col", ObjectInt $ R.col end)
                                                                            , ("virt_text", virtText)
                                                                            ]
      where
        virtText = ObjectArray [ ObjectArray [ ObjectString $ T.encodeUtf8 text
                                             , ObjectString "agdaHoleVirtualText"
                                             ]
                               ]
    setVirtualText _ _ = pure ()

dispatchResponse :: DispatchContext -> Response -> Neovim AgdaEnv ()
dispatchResponse _   (Status StatusInfo {}) = nvim_command "echo ''"
dispatchResponse ctx (InteractionPoints pts) = do
  (mark2id, id2marks) <- setInteractionMarks (agdaBuffer ctx) pts
  modifyPayload ctx $ \p -> p { markId2interactionPoint = mark2id
                              , interactionPoint2markIds = id2marks
                              }
dispatchResponse ctx (DisplayInfo AllGoalsWarnings { .. }) = do
  addMarks ctx (R.range :: RangeWithId -> [R.Range]) visibleGoals
  addMarks ctx (R.range :: RangeWithName -> [R.Range]) invisibleGoals
  setOutputBuffer ctx $ fmtGoals "Goals" (T.pack . show . getId . id'range) visibleGoals
                     <> fmtGoals "Invisible" name'range invisibleGoals
                     <> fmtMessages "Errors" errors
                     <> fmtMessages "Warnings" warnings
  where
    fmtGoals :: String -> (range -> T.Text) -> [Goal range] -> V.Vector T.Text
    fmtGoals name _        [] = V.fromList [ [i|#{name}: none|], " " ]
    fmtGoals name fmtRange goals = V.fromList ([i|#{name}:|] : (fmtGoal <$> goals)) <> V.singleton " "
      where
        fmtGoal goal = [i|?#{fmtRange $ constraintObj goal}: #{T.replace "\n" "\n    " $ fmtGoalType goal}|] :: T.Text
dispatchResponse _   (DisplayInfo Version {}) = pure ()
dispatchResponse ctx (DisplayInfo Error { .. })
  | null warnings = setOutputBuffer ctx (Identity $ message error')
  | otherwise = setOutputBuffer ctx $ fmtMessages "Error" [error']
                                   <> fmtMessages "Warnings" warnings
dispatchResponse ctx (DisplayInfo GoalSpecific { .. }) = dispatchGoalInfo ctx goalInfo
dispatchResponse ctx GiveAction { .. } = insertGivenResult ctx giveResult interactionPoint
dispatchResponse ctx MakeCase { .. } = handleMakeCase variant ctx clauses interactionPoint
dispatchResponse ctx (HighlightingInfo (HlInfo _ bits)) = handleHighlights ctx bits
dispatchResponse _   (RunningInfo _ msg) = nvim_command [i|echom '#{T.encodeUtf8 msg}'|]
dispatchResponse ctx ClearHighlighting = nvim_buf_clear_namespace (agdaBuffer ctx) (-1) 0 (-1)
dispatchResponse _   ClearRunningInfo = nvim_command "echo ''"
dispatchResponse _   JumpToError { .. } = pure ()

fmtGoalType :: Goal a -> T.Text
fmtGoalType OfType { .. } = type'goal
fmtGoalType JustSort {} = "Sort"

fmtMessages :: T.Text -> [Message] -> V.Vector T.Text
fmtMessages _    [] = mempty
fmtMessages name msgs = V.fromList $ name <> ":" : fmap message msgs


expandHoles :: T.Text -> T.Text
expandHoles = T.replace "?" "{! !}"

handleMakeCase :: String -> DispatchContext -> [T.Text] -> RangeWithId -> Neovim AgdaEnv ()
handleMakeCase "Function" ctx clauses = withRange ctx f
  where
    f start end = nvim_buf_set_lines (agdaBuffer ctx) (row start) (row end + 1) False $ V.fromList $ T.encodeUtf8 . expandHoles <$> clauses
handleMakeCase variant _ _ = const $ nvim_err_writeln [i|Unknown make case variant: #{variant}|]


insertGivenResult :: DispatchContext -> GiveResult -> RangeWithId -> Neovim AgdaEnv ()
insertGivenResult ctx GiveResult { .. } = withRange ctx f
  where
    f start end = nvim_buf_set_text (agdaBuffer ctx) (U.row start) (U.col start) (U.row end) (U.col end) (pure $ T.encodeUtf8 $ expandHoles str'given)


withRange :: DispatchContext -> (Cursor64 -> Cursor64 -> Neovim AgdaEnv ()) -> RangeWithId -> Neovim AgdaEnv ()
withRange ctx f range = iipRange ctx range >>= maybe (nvim_err_writeln [i|Unknown range: #{range}|]) (uncurry f)

iipRange :: DispatchContext -> RangeWithId -> Neovim AgdaEnv (Maybe (Cursor64, Cursor64))
iipRange ctx range = withPayload ctx $ \payload -> do
  goalmarksId <- asks goalmarksNs >>= readTVarIO
  marks <- nvim_buf_get_extmarks (agdaBuffer ctx) goalmarksId (ObjectInt 0) (ObjectInt (-1)) [("details", ObjectBool True)]
  pure $ do
    [markId] <- id'range range `HM.lookup` interactionPoint2markIds payload
    MarkObject { markId = _, .. } <- parseMarkObject =<< V.find (findById markId) marks
    pure (markStart, markEnd)
  where
    findById markId (ObjectArray ((ObjectInt markId') : _)) = markId == markId'
    findById _ _ = False


fmtGoalContextEntry :: GoalContextEntry -> T.Text
fmtGoalContextEntry GoalContextEntry { .. } = [i|#{originalName}#{reifyMarker}: #{binding}#{scopeMarker}|]
  where
    scopeMarker = if inScope then T.empty else " (not in scope)"
    reifyMarker = if originalName == reifiedName then "" else " (renamed to " <> reifiedName <> ")"

dispatchGoalInfo :: DispatchContext -> GoalInfo -> Neovim AgdaEnv ()
dispatchGoalInfo ctx GoalType { .. } = setOutputBuffer ctx $ V.fromList $ typeAuxInfo : header : (fmtGoalContextEntry <$> entries)
  where
    header = "Goal: " <> type'goal <> "\n" <> T.replicate 40 "-"
    typeAuxInfo = case typeAux of
                       Just GoalAndHave { .. } -> "Have: " <> expr
                       Just GoalAndElaboration { .. } -> "Elab: " <> term
                       _ -> ""
dispatchGoalInfo ctx CurrentGoal { .. } = setOutputBuffer ctx (Identity $ "Goal: " <> type'goal)


handleHighlights :: DispatchContext -> [HlBit] -> Neovim AgdaEnv ()
handleHighlights ctx bits = do
  highlightId <- asks highlightNs >>= readTVarIO
  p2c <- preparePosition2Cursor $ agdaBuffer ctx
  case concat <$> mapM (prepareHlBit p2c) bits of
       Left e    -> nvim_err_writeln e
       Right res -> void $ nvim_call_atomic $ serializePreparedHighlights (agdaBuffer ctx) highlightId res

data PreparedHighlight = PreparedHighlight
  { phAtom :: BS.ByteString
  , phRow :: Int64
  , phFromCol :: Int64
  , phToCol :: Int64
  } deriving (Eq, Show)

prepareHlBit :: Position2Cursor -> HlBit -> Either BS.ByteString [PreparedHighlight]
prepareHlBit pos2cur (HlBit atoms [fromPos, toPos])
  | Just from <- position2cursor pos2cur fromPos
  , Just to <- position2cursor pos2cur toPos = onRange from to highlight
  | otherwise = Right []
  where
    highlight row fromCol toCol = Right $ (\atom -> PreparedHighlight (T.encodeUtf8 atom) row (fromMaybe 0 fromCol) (fromMaybe (-1) toCol)) <$> atoms
prepareHlBit _       (HlBit _     range) = Left [i|Unexpected range format: #{range}|]

serializePreparedHighlights :: Buffer -> Int64 -> [PreparedHighlight] -> V.Vector Object
serializePreparedHighlights buf hlNs = V.fromList . fmap (\ph -> ObjectArray [ObjectString "nvim_buf_add_highlight", args ph])
  where
    args PreparedHighlight { .. } = ObjectArray [ toObject buf
                                                , ObjectInt hlNs
                                                , ObjectString $ "agda_atom_" <> phAtom
                                                , ObjectInt phRow
                                                , ObjectInt phFromCol
                                                , ObjectInt phToCol
                                                ]

data Position2Cursor = Position2Cursor
  { linesOffsets :: [Int64]
  , linesContents :: [T.Text]
  }

preparePosition2Cursor :: Buffer -> Neovim AgdaEnv Position2Cursor
preparePosition2Cursor buf = do
  linesContents <- toList . fmap T.decodeUtf8 <$> nvim_buf_get_lines buf 0 (-1) False
  let linesOffsets = scanl (\acc str -> acc + fromIntegral (T.length str) + 1) 0 $ toList linesContents
  pure Position2Cursor { .. }

position2cursor :: Position2Cursor -> Int64 -> Maybe Cursor64
position2cursor Position2Cursor { .. } pos = do
  lineIdx <- subtract 1 <$> findIndex (>= pos) linesOffsets
  let lineOffset = linesOffsets !! lineIdx
      colIdx = linesContents !! lineIdx @| pos - lineOffset - 1
  pure $ Cursor (fromIntegral lineIdx) colIdx


setOutputBuffer :: Foldable f => DispatchContext -> f T.Text -> Neovim env ()
setOutputBuffer ctx = nvim_buf_set_lines (outputBuffer ctx) 0 (-1) False
                    . V.concatMap (V.fromList . BS.split '\n' . T.encodeUtf8)
                    . V.fromList
                    . toList
