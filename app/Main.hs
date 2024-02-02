module Main where

import AutoPrinter
import GHC.Generics (Generic, from)
import qualified Data.Text.Lazy.IO as TextIO

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Generic, Show)

-- data RoseTree a = Rose a [RoseTree a]
-- Would need to figure out how to handle "default cases" too

instance ShowBuilder a => ShowBuilder (Tree a) where

test :: Tree Int
test = Node (Node Leaf 0 Leaf) 1 (Node (Node Leaf 2 Leaf) 3 Leaf)

main :: IO ()
main = do
  print (from test)
  print test
  TextIO.putStrLn (showText test)
