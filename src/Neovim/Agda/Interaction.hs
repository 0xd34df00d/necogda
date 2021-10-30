{-# LANGUAGE RecordWildCards, OverloadedStrings, OverloadedLists #-}

module Neovim.Agda.Interaction
( module X

, setInteractionMarks
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
