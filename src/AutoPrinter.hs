module AutoPrinter where

import GHC.Generics
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText, fromString)

class ShowBuilder a where
  build :: a -> Builder
  default build :: (Generic a, ShowBuilder1 (Rep a)) => a -> Builder
  build = build1 . from

class ShowBuilder1 f where
  build1 :: f p -> Builder

instance ShowBuilder Int where
  build = fromString . show

showText :: ShowBuilder a => a -> Text
showText = toLazyText . build

instance ShowBuilder1 V1 where
  build1 _ = mempty

instance ShowBuilder1 U1 where
  build1 _ = mempty

instance ShowBuilder c => ShowBuilder1 (K1 i c) where
  build1 (K1 x) = build x

instance ShowBuilder1 f => ShowBuilder1 (M1 i c f) where
  build1 (M1 x) = build1 x

instance (ShowBuilder1 f, ShowBuilder1 g) => ShowBuilder1 (f :+: g) where
  build1 (L1 x) = build1 x
  build1 (R1 x) = build1 x

instance (ShowBuilder1 f, ShowBuilder1 g) => ShowBuilder1 (f :*: g) where
  build1 (x :*: y) = build1 x <> build1 y
