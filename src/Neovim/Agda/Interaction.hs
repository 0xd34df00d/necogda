{-# LANGUAGE RecordWildCards, OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Neovim.Agda.Interaction
( module Req

, setInteractionMarks
, getCurrentInteractionId
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Identity
import Data.Foldable
import Data.Maybe
import UnliftIO

import qualified Neovim.API.Text as AT
import Neovim
import Neovim.API.ByteString

import Neovim.Agda.Response.Types as Resp
import Neovim.Agda.Request.Types as Req
import Neovim.Agda.Types
import Neovim.Agda.Util as U

setInteractionMarks :: Buffer -> [RangeWithId] -> Neovim AgdaEnv (MarkId2InteractionPoint, InteractionPoint2MarkIds)
setInteractionMarks buffer pts = do
  goalmarksId <- asks goalmarksNs >>= readTVarIO
  nvim_buf_clear_namespace buffer goalmarksId 0 (-1)

  ls <- AT.nvim_buf_get_lines (convertAPI buffer) 0 (-1) False

  id'range2markIds <- forM pts $ \RangeWithId { .. } -> do
    markIds <- forM range $ \Resp.Range { .. } -> do
      let startLine = line start - 1
          startCol  = ls V.! fromIntegral startLine @| Resp.col start - 1
          endLine = line end - 1
          endCol  = ls V.! fromIntegral endLine @| Resp.col end - 1
          extraOpts = M.fromList [ ("end_line", ObjectInt endLine)
                                 , ("end_col",  ObjectInt endCol)
                                 ]
      nvim_buf_set_extmark buffer goalmarksId startLine startCol extraOpts
      -- TODO attach virtual text to it when it's available in AllGoalsWarnings
    pure (id'range, markIds)
  let mark2id = HM.fromList [ (markId, id'range)
                            | (id'range, markIds) <- id'range2markIds
                            , markId <- markIds
                            ]
  let id2marks = HM.fromList id'range2markIds
  pure (mark2id, id2marks)

maybeFallback :: (a -> Maybe a) -> a -> a
maybeFallback f txt = fromMaybe txt $ f txt

getCurrentInteractionId :: AgdaInstance -> Neovim AgdaEnv (Maybe (InteractionId, T.Text))
getCurrentInteractionId AgdaInstance { .. } = do
  goalmarksId <- asks goalmarksNs >>= readTVarIO

  buf <- nvim_get_current_buf
  win <- nvim_get_current_win
  (curRow, curCol) <- nvim_win_get_cursor win
  marks <- nvim_buf_get_extmarks buf goalmarksId (ObjectInt 0) (ObjectInt (-1)) [("details", ObjectBool True)]
  let extractLines (iid, (from, to)) = do
        ls <- nvim_buf_get_lines buf (fromIntegral $ row from) (fromIntegral $ row to + 1) False
        let origText = BS.strip $ runIdentity $ onRange from to $ extractLine (row from) ls
        let text = if origText == "?"
                   then mempty
                   else BS.strip $ maybeFallback (BS.stripPrefix "{!") $ maybeFallback (BS.stripSuffix "!}") origText
        pure (iid, T.decodeUtf8 text)
  traverse extractLines $ findMark (markId2interactionPoint payload) (curRow - 1) curCol marks
  where
    extractLine start ls row maybeStart maybeEnd = pure $ maybe id BS.drop maybeStart
                                                        $ maybe id BS.take maybeEnd
                                                        $ ls V.! (row - start)

findMark :: Foldable f => MarkId2InteractionPoint -> Int64 -> Int64 -> f Object -> Maybe (InteractionId, (Cursor, Cursor))
findMark id2ip row col = listToMaybe . mapMaybe f . toList >=> getInteractionId
  where
    f (ObjectArray [ ObjectInt markId
                   , ObjectInt markRow
                   , ObjectInt markCol
                   , ObjectMap extras
                   ])
      | Just (ObjectInt endRow) <- ObjectString "end_row" `M.lookup` extras
      , Just (ObjectInt endCol) <- ObjectString "end_col" `M.lookup` extras
      , (row, col) `between` ((markRow, markCol), (endRow, endCol)) = Just (markId, (fromIntegral <$> Cursor markRow markCol, fromIntegral <$> Cursor endRow endCol))
    f _ = Nothing

    getInteractionId :: (Int64, a) -> Maybe (InteractionId, a)
    getInteractionId (markId, a) = (, a) . InteractionId . getId <$> markId `HM.lookup` id2ip

between :: Ord a => a -> (a, a) -> Bool
smth `between` (start, end) = start <= smth && smth <= end
