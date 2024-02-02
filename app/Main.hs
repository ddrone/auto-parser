module Main where

import AutoPrinter
import GHC.Generics (Generic)
import qualified Data.Text.Lazy.IO as TextIO

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Generic)

-- data RoseTree a = Rose a [RoseTree a]
-- Would need to figure out how to handle "default cases" too

instance ShowBuilder a => ShowBuilder (Tree a) where

test :: Tree Int
test = Node (Node Leaf 0 Leaf) 1 (Node (Node Leaf 2 Leaf) 3 Leaf)

main :: IO ()
main = do
  TextIO.putStrLn (showText (10 :: Int))
  TextIO.putStrLn (showText test)
