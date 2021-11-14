{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Bifunctor
import Data.Data
import Data.Generics.Uniplate.Data
import Data.List
import Data.SExpresso.Parse
import Data.SExpresso.SExpr
import Data.SExpresso.Language.SchemeR5RS
import Data.Void
import System.Environment
import Text.Megaparsec as M

deriving instance Data SchemeToken
deriving instance Data SchemeNumber
deriving instance Data Complex
deriving instance Data Exactness
deriving instance Data SReal
deriving instance Data UInteger
deriving instance Data Sign
deriving instance Data SExprType
deriving instance Data Suffix
deriving instance Data Precision
deriving instance (Data a, Data b) => Data (SExpr a b)

type SchemeExpr = SExpr SExprType SchemeToken

handleSExpr :: [SchemeExpr] -> Either String [(T.Text, [T.Text])]
handleSExpr sexprs
  | [SList _ defRoot] <- [ defs | SList _ (SAtom (TIdentifier "defcustom") : SAtom (TIdentifier "agda-input-translations") : (defs :: SchemeExpr) : _) <- universeBi sexprs ]
  , [SList _ defs] <- tail $ dropWhile (/= SAtom TQuasiquote) defRoot = mapM f defs
  | otherwise = Left "not found"
  where
    f (SList _ [SAtom (TString abbrev), SAtom TDot,               SList _ codes]) = pure (abbrev, [ code | SAtom (TString code) <- codes ])
    f (SList _ [SAtom (TString abbrev), SAtom TDot, SAtom TComma, SList _ [SAtom (TIdentifier "agda-input-to-string-list"), SAtom (TString codes)]]) = pure (abbrev, toStringList codes)
    f elt = Left $ "unknown def: " <> show elt

    toStringList = fmap (T.pack . pure) . filter (`notElem` [' ', '\n']) . T.unpack

fixupBackslash :: String -> String
fixupBackslash ('\\' : '"' : rest)  = '\\' : '"' : fixupBackslash rest
fixupBackslash ('\\' : '\\' : rest) = '\\' : '\\' : fixupBackslash rest
fixupBackslash ('\\' : c : rest) = '\\' : '\\' : c : fixupBackslash rest
fixupBackslash (c : rest) = c : fixupBackslash rest
fixupBackslash [] = []

handleEl :: FilePath -> IO ()
handleEl elFile  = do
  elContents <- readFile elFile
  either putStrLn (printCodes . sort) $ handleSExpr =<< first prettyParseError (parse (decode sexpr) elFile $ fixupBackslash elContents)
  where
    prettyParseError err = "Parse error:\n" <> errorBundlePretty @_ @Void err
    printCodes = mapM_ $ \(abbrev, codes) -> T.putStrLn $ T.unwords (abbrev : sort codes)


main :: IO ()
main = getArgs >>=
  \case [path] -> handleEl path
        _ -> putStrLn "Usage: tablegen <path to .el>"
