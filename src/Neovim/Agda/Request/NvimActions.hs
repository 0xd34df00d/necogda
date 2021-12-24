{-# LANGUAGE RankNTypes #-}

module Neovim.Agda.Request.NvimActions
( goalNext
, goalPrev
) where

import qualified Data.Vector as V
import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord

import Neovim
import Neovim.API.ByteString

import Neovim.Agda.Interaction
import Neovim.Agda.Types
import Neovim.Agda.Util

goalNav :: Ord s => (MarkObject -> s) -> (Cursor64 -> MarkObject -> Bool) -> Neovim AgdaEnv ()
goalNav sorter cmpWith = do
  buf <- nvim_get_current_buf
  win <- nvim_get_current_win
  marks <- sortOn sorter . V.toList <$> getGoalMarks buf
  unless (null marks) $ do
    cur <- uncurry Cursor . first (subtract 1) <$> nvim_win_get_cursor win
    let nextCur = markStart $ fromMaybe (head marks) $ find (cmpWith cur) marks
    nvim_win_set_cursor win $ first (+ 1) $ curToPair nextCur

goalNext, goalPrev :: Neovim AgdaEnv ()
goalNext = goalNav id   $ \cur markObj -> markStart markObj > cur
goalPrev = goalNav Down $ \cur markObj -> markEnd   markObj < cur
