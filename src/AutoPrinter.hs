module AutoPrinter where

import GHC.Generics
import Data.Text.Lazy.Builder (Builder)

class ShowBuilder a where
  build :: a -> Builder
  default build :: (Generic a, ShowBuilder1 (Rep a)) => a -> Builder
  build = build1 . from

class ShowBuilder1 f where
  build1 :: f p -> Builder
