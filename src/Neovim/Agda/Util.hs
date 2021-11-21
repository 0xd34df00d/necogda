{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Neovim.Agda.Util where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.Extra (mconcatMapM)

import Neovim
import qualified Neovim.API.ByteString as AB
import qualified Neovim.API.Text as AT

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

class ConvertAPI from to where
  convertAPI :: from -> to

instance ConvertAPI AB.Buffer AT.Buffer where
  convertAPI (AB.Buffer bs) = AT.Buffer bs

instance ConvertAPI AT.Buffer AB.Buffer where
  convertAPI (AT.Buffer bs) = AB.Buffer bs

infix 1 @|
(@|) :: (Integral a, Num b) => T.Text -> a -> b
str @| pos = fromIntegral $ BS.length $ T.encodeUtf8 $ T.take (fromIntegral pos) str
