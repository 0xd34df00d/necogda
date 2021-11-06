{-# LANGUAGE QuasiQuotes #-}

module Neovim.Agda.Util where

import Control.Monad.Extra (mconcatMapM)

import Neovim

onRange :: Monoid r
        => (Int64, Int64)
        -> (Int64, Int64)
        -> (Int64 -> Maybe Int64 -> Maybe Int64 -> Neovim env r)
        -> Neovim env r
onRange (fromRow, fromCol) (toRow, toCol) f
  | fromRow == toRow = f fromRow (Just fromCol) (Just toCol)
  | otherwise = do
      s <- f fromRow (Just fromCol) Nothing
      mid <- mconcatMapM (\row -> f row Nothing Nothing) [fromRow + 1 .. toRow - 1]
      e <- f toRow Nothing (Just toCol)
      pure $ s <> mid <> e
