module Playground where

import GHC.Generics (Generic, from)

import AutoPrinter
import AutoParser (TextParser)

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Generic, Show)

data RoseTree a = Rose a [RoseTree a]
  deriving (Generic, Show)

data Foo = Foo
  { bar :: Int
  , baz :: Int
  , quux :: [Int]
  }
  deriving (Generic, Show)

data Foo2 = Foo2 Foo Foo
  deriving (Generic, Show)

instance ShowBuilder a => ShowBuilder (Tree a) where
instance ShowBuilder a => ShowBuilder (RoseTree a) where
instance TextParser a => TextParser (Tree a) where
instance TextParser a => TextParser (RoseTree a) where
instance ShowBuilder Foo

test :: Tree Int
test = Node (Node Leaf 0 Leaf) 1 (Node (Node Leaf 2 Leaf) 3 Leaf)

rose :: RoseTree Int
rose = Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 []], Rose 6 []]

foo :: Foo
foo = Foo 1 2 [3, 4, 5]

-- Surprisingly, one does not need brackets around records in this case
foo2 :: Foo2
foo2 = Foo2 Foo { bar = 1, baz = 2, quux = [] } Foo { bar = 3, baz = 4, quux = [5, 6] }
