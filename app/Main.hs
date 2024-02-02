module Main where

import AutoPrinter
import Playground
import GHC.Generics (Generic, from)
import qualified Data.Text.Lazy.IO as TextIO

main :: IO ()
main = do
  print test
  TextIO.putStrLn (showText test)
  print rose
  TextIO.putStrLn (showText rose)
