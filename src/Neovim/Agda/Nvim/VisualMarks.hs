{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Neovim.Agda.Nvim.VisualMarks where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad
import Data.String
import Data.String.Interpolate.IsString
import GHC.Records
import UnliftIO

import Neovim
import Neovim.API.ByteString

import Neovim.Agda.Response.Types as R
import Neovim.Agda.Types
import Neovim.Agda.Util as U

data VMKind = VMGoal | VMWarning | VMError deriving (Eq, Show, Enum, Bounded)

data VirtualMark = VirtualMark
  { vmStart :: Cursor64
  , vmEnd :: Cursor64
  , vmText :: T.Text
  , vmKind :: VMKind
  }

kindSymbol :: VMKind -> BS.ByteString
kindSymbol = T.encodeUtf8 . \case VMGoal    -> "⛳"
                                  VMWarning -> "⚠"
                                  VMError   -> "❌"

kindSignName :: VMKind -> BS.ByteString
kindSignName kind = "agdaSign" <> fromString (show kind)

kindHighlightName :: VMKind -> BS.ByteString
kindHighlightName kind = "agda" <> fromString (show kind)

signGroupName :: BS.ByteString
signGroupName = "agda-vms"

addVirtualMarks :: Buffer -> [VirtualMark] -> Neovim AgdaEnv ()
addVirtualMarks buf marks = do
  vmId <- asks virtualMarksNs >>= readTVarIO
  forM_ marks $ \VirtualMark { .. } -> do
    void $ nvim_exec [i|call sign_place(0, "#{signGroupName}", "#{kindSignName vmKind}", "%", { "lnum": #{U.row vmStart + 1} })|] False

    let textObj = ObjectArray [ ObjectArray [ ObjectString $ "  " <> kindSymbol vmKind <> " " <> BS.map (\ch -> if ch == '\n' then ' ' else ch) (T.encodeUtf8 vmText)
                                            , ObjectArray [ObjectString $ kindHighlightName vmKind, ObjectString "agdaItalic"]
                                            ]
                              ]
    nvim_buf_set_extmark buf vmId (U.row vmStart) (U.col vmStart) [ ("end_line", ObjectInt $ U.row vmEnd)
                                                                  , ("end_col", ObjectInt $ U.col vmEnd)
                                                                  , ("virt_text", textObj)
                                                                  ]


-- * Various helpers for specific mark types

addGoalMarks :: HasField "range" r [Range] => Buffer -> (Goal r -> T.Text) -> [Goal r] -> Neovim AgdaEnv ()
addGoalMarks buf fmtGoalType goals = addVirtualMarks buf [ VirtualMark
                                                           { vmStart = Cursor (R.line start - 1) (R.col start)
                                                           , vmEnd   = Cursor (R.line end - 1)   (R.col end)
                                                           , vmText  = fmtGoalType goal
                                                           , vmKind  = VMGoal
                                                           }
                                                         | goal <- goals
                                                         , let R.Range { .. } = head $ getField @"range" $ constraintObj goal
                                                         ]

atomsMarkInfo :: [T.Text] -> Maybe (T.Text, VMKind)
atomsMarkInfo = msum . fmap (`HM.lookup` atom2message)
  where
    atom2message = [ ("error", ("Error", VMError))
                   , ("unsolvedmeta", ("Unsolved meta", VMWarning))
                   , ("unsolvedconstraint", ("Unsolved constraint", VMWarning))
                   , ("terminationproblem", ("Termination problem", VMError))
                   , ("deadcode", ("Dead code", VMWarning))
                   , ("coverageproblem", ("Coverage problem", VMError))
                   , ("positivityproblem", ("Positivity problem", VMError))
                   , ("incompletepattern", ("Incomplete pattern", VMError))
                   , ("confluenceproblem", ("Confluence problem", VMError))
                   , ("missingdefinition", ("Missing definition", VMError))
                   ]
