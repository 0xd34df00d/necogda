{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Data.Bifunctor
import Data.Functor
import Data.Generics.Uniplate.Data
import Data.List
import Data.Proxy
import Data.SExpresso.Parse
import Data.SExpresso.SExpr
import Data.SExpresso.Language.SchemeR5RS
import Data.String
import Data.Void
import System.Environment
import Text.Megaparsec as M
import Text.Megaparsec.Char as M

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

data VimAST
  = VimArray [VimAST]
  | VimObj [(String, VimAST)]
  | VimStr String
  deriving (Eq, Ord, Show)

vimASTParser :: forall e s m. (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m VimAST
vimASTParser = parseArray <|> parseObj <|> parseStr
  where
    parseArray = VimArray <$> between (char '[') (char ']') (vimASTParser `sepBy` string ", ")
    parseObj = VimObj <$> between (char '{') (char '}') (parseKVPair `sepBy` string ", ")
    parseKVPair = do
     key <- between (char q) (string "': ") $ many $ anySingleBut q <|> (string "''" $> q)
     val <- vimASTParser
     pure (key, val)
    parseQuoted = chunkToTokens (Proxy :: Proxy s) <$> between (char q) (char q) (takeWhile1P Nothing (/= q))
    parseStr = VimStr <$> parseQuoted

    q = '\''

collectVimTrie :: VimAST -> HM.HashMap String [String]
collectVimTrie = go ""
  where
    go prefix (VimArray arr) = union' $ go prefix <$> arr
    go prefix (VimObj pairs) = union' $ (\(k, v) -> go (prefix <> k) v) <$> pairs
    go prefix (VimStr s) = HM.singleton prefix [s]
    union' = foldl' (HM.unionWith (<>)) mempty

handleVimTrie :: FilePath -> IO ()
handleVimTrie path = do
  vimContents <- readFile path
  case parse vimASTParser path vimContents of
       Left err -> putStrLn $ errorBundlePretty @_ @Void err
       Right res -> forM_ (sort $ HM.toList $ collectVimTrie res) $ \(_:k, vs) -> putStrLn $ unwords (k : sort vs)

main :: IO ()
main = getArgs >>=
  \case ["el", path] -> handleEl path
        ["vt", path] -> handleVimTrie path
        _            -> putStrLn "Usage: tablegen (el <path to .el> | vt <path to vim trie>)"
