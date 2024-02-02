module Main where

import AutoPrinter
import GHC.Generics (Generic, from)
import qualified Data.Text.Lazy.IO as TextIO

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Generic, Show)

data RoseTree a = Rose a [RoseTree a]
  deriving (Generic, Show)

instance ShowBuilder a => ShowBuilder (Tree a) where

test :: Tree Int
test = Node (Node Leaf 0 Leaf) 1 (Node (Node Leaf 2 Leaf) 3 Leaf)

rose :: RoseTree Int
rose = Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 []], Rose 6 []]

instance ShowBuilder a => ShowBuilder (RoseTree a) where

main :: IO ()
main = do
  print test
  TextIO.putStrLn (showText test)
  print rose
  TextIO.putStrLn (showText rose)
