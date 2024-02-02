module AutoPrinter where

import Data.Proxy (Proxy(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText, fromString, singleton)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)

class ShowBuilder a where
  build :: Bool -> a -> Builder
  default build :: (Generic a, ShowBuilder1 (Rep a)) => Bool -> a -> Builder
  build b = build1 b . from

class ShowBuilder1 f where
  build1 :: Bool -> f p -> Builder

instance ShowBuilder Int where
  build _ = fromString . show

instance ShowBuilder a => ShowBuilder [a] where
  build _ ls = case ls of
    [] -> fromString "[]"
    [x] -> singleton '[' <> build False x <> singleton ']'
    x : rest -> singleton '[' <> go x rest
    where
      go x [] = build False x <> singleton ']'
      go x (y : rest) = build False x <> fromString ", " <> go y rest

showText :: ShowBuilder a => a -> Text
showText = toLazyText . build False

instance ShowBuilder1 U1 where
  build1 _ = mempty

instance ShowBuilder c => ShowBuilder1 (K1 i c) where
  build1 _ (K1 x) = build True x

constructorName :: KnownSymbol s => M1 i (MetaCons s fi b) f a -> String
constructorName (_ :: M1 i (MetaCons s fi b) f a) = symbolVal (Proxy :: Proxy s)

instance (KnownSymbol s, ShowBuilder1 f) => ShowBuilder1 (M1 i (MetaCons s fi b) f) where
  build1 b y@(M1 x) = wrap (fromString (constructorName y) <> build1 False x)
    where
      wrap builder = if b then singleton '(' <> builder <> singleton ')' else builder

instance ShowBuilder1 f => ShowBuilder1 (M1 i (MetaData s1 s2 b1 b2) f) where
  build1 b (M1 x) = build1 b x

instance ShowBuilder1 f => ShowBuilder1 (M1 i (MetaSel ms su ss ds) f) where
  build1 b (M1 x) = build1 b x

instance (ShowBuilder1 f, ShowBuilder1 g) => ShowBuilder1 (f :+: g) where
  build1 b (L1 x) = build1 b x
  build1 b (R1 x) = build1 b x

instance (ShowBuilder1 f, ShowBuilder1 g) => ShowBuilder1 (f :*: g) where
  build1 b (x :*: y) = singleton ' ' <> build1 b x <> build1 b y
