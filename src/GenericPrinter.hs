{-# OPTIONS_GHC -Wno-missing-signatures #-}
module GenericPrinter where

import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

-- This module isn't really necessary for the generic printer itself, but is more of a condensed version
-- of the actual pretty-printing algorithm stripped of all the extra details, so that I would be able
-- to refer back to it if necessary.

data Assoc
  = AssocLeft
  | AssocRight
  deriving (Eq, Show)

data Op = Op
  { opText :: Text
  , opPriority :: Int
  , opAssoc :: Assoc
  }
  deriving (Show)

data Expr
  = Atom Text
  | Bin Expr Op Expr
  deriving (Show)

add = Op "+" 4 AssocLeft
mul = Op "*" 6 AssocLeft
apply = Op "" 99 AssocLeft

test1 = Bin (Atom "2") add (Bin (Atom "2") mul (Atom "2"))
test2 = Bin (Atom "2") add (Bin (Atom "2") add (Atom "2"))
test3 = Bin (Bin (Atom "2") add (Atom "2")) add (Atom "2")
test4 = Bin (Atom "2") mul (Bin (Atom "2") add (Atom "2"))

-- Taking a parent operator and whether the current child is left or right,
-- prettily print the expression with minimal amount of brackets
printExpr :: Maybe (Op, Assoc) -> Expr -> Builder
printExpr path expr = case expr of
  Atom t -> fromText t
  Bin left op right -> wrap (printExpr (Just (op, AssocLeft)) left <> printOp <> printExpr (Just (op, AssocRight)) right)
    where
      printOp =
        if Text.null (opText op)
          then singleton ' '
          else singleton ' ' <> fromText (opText op) <> singleton ' '
      putBrackets = case path of
        Nothing -> False
        Just (parentOp, ctx) -> (opPriority parentOp > opPriority op) ||
          opPriority parentOp == opPriority op && ctx /= opAssoc parentOp
      wrap x =
        if putBrackets
          then singleton '(' <> x <> singleton ')'
          else x

printToplevel :: Expr -> Text
printToplevel = LazyText.toStrict . toLazyText . printExpr Nothing
