{-# LANGUAGE RecordWildCards, OverloadedStrings, OverloadedLists #-}

module Neovim.Agda.Interaction
( module X

, setInteractionMarks
, getCurrentInteractionId
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Control.Monad
import Data.Foldable
import Data.Maybe
import UnliftIO

import Neovim
import Neovim.API.ByteString

import Neovim.Agda.Interaction.Types as X
import Neovim.Agda.Response as R
import Neovim.Agda.Types

setInteractionMarks :: Buffer -> [RangeWithId] -> Neovim AgdaEnv MarkId2InteractionPoint
setInteractionMarks buffer pts = do
  goalmarksId <- asks goalmarksNs >>= readTVarIO
  nvim_buf_clear_namespace buffer goalmarksId 0 (-1)

  id'range2markIds <- forM pts $ \RangeWithId { .. } -> do
    markIds <- forM range $ \R.Range { .. } -> do
      let startLine = line start - 1
          startCol  = col start - 1
          endLine = line end - 1
          endCol  = col end - 1
          extraOpts = M.fromList [ ("end_line", ObjectInt endLine)
                                 , ("end_col",  ObjectInt endCol)
                                 ]
      nvim_buf_set_extmark buffer goalmarksId startLine startCol extraOpts
      -- TODO attach virtual text to it when it's available in AllGoalsWarnings
    pure (id'range, markIds)
  pure $ HM.fromList [ (markId, id'range)
                     | (id'range, markIds) <- id'range2markIds
                     , markId <- markIds
                     ]

getCurrentInteractionId :: AgdaInstance -> Neovim AgdaEnv (Maybe InteractionId)
getCurrentInteractionId AgdaInstance { .. } = do
  goalmarksId <- asks goalmarksNs >>= readTVarIO

  buf <- nvim_get_current_buf
  win <- nvim_get_current_win
  (row, col) <- nvim_win_get_cursor win
  marks <- nvim_buf_get_extmarks buf goalmarksId (ObjectInt 0) (ObjectInt (-1)) [("details", ObjectBool True)]
  let mark = findMark (markId2interactionPoint payload) (row - 1) col marks
  pure mark

findMark :: Foldable f => MarkId2InteractionPoint -> Int64 -> Int64 -> f Object -> Maybe InteractionId
findMark id2ip row col = (listToMaybe . mapMaybe f . toList) >=> getInteractionId
  where
    f (ObjectArray [ ObjectInt markId
                   , ObjectInt markRow
                   , ObjectInt markCol
                   , ObjectMap extras
                   ])
      | Just (ObjectInt endRow) <- ObjectString "end_row" `M.lookup` extras
      , Just (ObjectInt endCol) <- ObjectString "end_col" `M.lookup` extras
      , (row, col) `between` ((markRow, markCol), (endRow, endCol)) = Just markId
    f _ = Nothing

    getInteractionId :: Int64 -> Maybe InteractionId
    getInteractionId markId = InteractionId . getId <$> markId `HM.lookup` id2ip

between :: Ord a => a -> (a, a) -> Bool
smth `between` (start, end) = start <= smth && smth <= end
