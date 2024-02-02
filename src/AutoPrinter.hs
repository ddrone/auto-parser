module AutoPrinter where

import Data.Proxy (Proxy(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText, fromString, singleton)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)

class ShowBuilder a where
  build :: Bool -> Maybe String -> a -> Builder
  default build :: (Generic a, ShowBuilder1 (Rep a)) => Bool -> Maybe String -> a -> Builder
  build b con = build1 b con . from

class ShowBuilder1 f where
  build1 :: Bool -> Maybe String -> f p -> Builder

instance ShowBuilder Int where
  build _ _ = fromString . show

instance ShowBuilder a => ShowBuilder [a] where
  -- TODO: do I need to do the wrapping here as well? For something like
  -- data T = T [String]
  build _ _ ls = case ls of
    [] -> fromString "[]"
    [x] -> singleton '[' <> build False Nothing x <> singleton ']'
    x : rest -> singleton '[' <> go x rest
    where
      go x [] = build False Nothing x <> singleton ']'
      go x (y : rest) = build False Nothing x <> fromString ", " <> go y rest

showText :: ShowBuilder a => a -> Text
showText = toLazyText . build False Nothing

instance ShowBuilder1 U1 where
  build1 _ con _ = maybe mempty fromString con

instance ShowBuilder c => ShowBuilder1 (K1 i c) where
  build1 m con (K1 x) = build m con x

constructorName :: KnownSymbol s => M1 i (MetaCons s fi b) f a -> String
constructorName (_ :: M1 i (MetaCons s fi b) f a) = symbolVal (Proxy :: Proxy s)

instance (KnownSymbol s, ShowBuilder1 f) => ShowBuilder1 (M1 i (MetaCons s fi b) f) where
  build1 b con y@(M1 x) = build1 True (Just (constructorName y)) x

instance ShowBuilder1 f => ShowBuilder1 (M1 i (MetaData s1 s2 b1 b2) f) where
  build1 b con (M1 x) = build1 b con x

instance ShowBuilder1 f => ShowBuilder1 (M1 i (MetaSel ms su ss ds) f) where
  build1 b con (M1 x) = build1 b con x

instance (ShowBuilder1 f, ShowBuilder1 g) => ShowBuilder1 (f :+: g) where
  build1 b con (L1 x) = build1 b con x
  build1 b con (R1 x) = build1 b con x

instance (ShowBuilder1 f, ShowBuilder1 g) => ShowBuilder1 (f :*: g) where
  build1 b con (x :*: y) = case con of
    Nothing -> build1 b Nothing x <> singleton ' ' <> build1 b Nothing y
    Just n -> wrap (fromString n <> singleton ' ' <> build1 b Nothing x <> singleton ' ' <> build1 b Nothing y)
    where
      wrap builder = if b then singleton '(' <> builder <> singleton ')' else builder
