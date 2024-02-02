module Main where

import GHC.Generics (Generic)

data Tree a
  = Node a [Tree a]
  deriving (Generic)

test :: Tree Int
test = Node 1 [Node 2 [], Node 3 [Node 4 []], Node 5 []]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
