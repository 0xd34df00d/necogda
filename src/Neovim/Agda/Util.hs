{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}

module Neovim.Agda.Util where

import Control.Monad.Extra (mconcatMapM)

import Neovim

data CursorT a = Cursor { row :: a, col :: a }
  deriving (Eq, Ord, Show, Functor)

type Cursor = CursorT Int
type Cursor64 = CursorT Int64

onRange :: (Monad m, Monoid r, Eq a, Enum a, Num a)
        => CursorT a
        -> CursorT a
        -> (a -> Maybe a -> Maybe a -> m r)
        -> m r
onRange from to f
  | row from == row to = f (row from) (Just $ col from) (Just $ col to)
  | otherwise = do
      s <- f (row from) (Just $ col from) Nothing
      e <- f (row to)   Nothing           (Just $ col to)
      mid <- mconcatMapM (\r -> f r Nothing Nothing) [row from + 1 .. row to - 1]
      pure $ s <> mid <> e
