module Playground where

import GHC.Generics (Generic, from)

import AutoPrinter

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Generic, Show)

data RoseTree a = Rose a [RoseTree a]
  deriving (Generic, Show)

instance ShowBuilder a => ShowBuilder (Tree a) where
instance ShowBuilder a => ShowBuilder (RoseTree a) where

test :: Tree Int
test = Node (Node Leaf 0 Leaf) 1 (Node (Node Leaf 2 Leaf) 3 Leaf)

rose :: RoseTree Int
rose = Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 []], Rose 6 []]