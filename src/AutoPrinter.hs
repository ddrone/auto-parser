module AutoPrinter where

import Data.Proxy (Proxy(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText, fromString, singleton)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)

data ShowContext = ShowContext
  { scNested :: Bool
  , scRecord :: Bool
  }

defContext :: ShowContext
defContext = ShowContext False False

class ShowBuilder a where
  build :: ShowContext -> a -> Builder
  default build :: (Generic a, ShowBuilder1 (Rep a)) => ShowContext -> a -> Builder
  build b = build1 b . from
  isProduct :: Proxy a -> Bool

class ShowBuilder1 f where
  build1 :: ShowContext -> f p -> Builder
  -- We're only using those to distunguish between U1 and products inside constructors
  -- The point of doing that is to figure out whether the brackets should be displayed
  -- when printing out the constructor name. Currently, a lot of the implementations of
  -- isProduct1/isProduct might not actually make sense, but at least it works.
  -- It would be nice to figure out the proper invariant and carefully maintain it, but
  -- doing this style of hasochistic programming is painful and I don't want to think
  -- about it more that absolutely necessary.
  isProduct1 :: Proxy (f p) -> Bool

instance ShowBuilder Int where
  build _ = fromString . show
  isProduct _ = False

instance ShowBuilder a => ShowBuilder [a] where
  -- TODO: do I need to do the wrapping here as well? For something like
  -- data T = T [String]
  build _ ls = case ls of
    [] -> fromString "[]"
    [x] -> singleton '[' <> build defContext x <> singleton ']'
    x : rest -> singleton '[' <> go x rest
    where
      go x [] = build defContext x <> singleton ']'
      go x (y : rest) = build defContext x <> fromString ", " <> go y rest
  isProduct _ = False

showText :: ShowBuilder a => a -> Text
showText = toLazyText . build defContext

instance ShowBuilder1 U1 where
  build1 _ _ = mempty
  isProduct1 _ = False

instance ShowBuilder c => ShowBuilder1 (K1 i c) where
  build1 ctx (K1 x) = build ctx { scNested = True } x
  isProduct1 _ = isProduct (Proxy :: Proxy c)

constructorName :: KnownSymbol s => M1 i (MetaCons s fi b) f a -> String
constructorName (_ :: M1 i (MetaCons s fi b) f a) = symbolVal (Proxy :: Proxy s)

instance (KnownSymbol s, ShowBuilder1 f) => ShowBuilder1 (M1 i (MetaCons s fi False) f) where
  build1 ctx y@(M1 x) =
    if prod
      then wrap (fromString (constructorName y ++ " ") <> build1 ctx { scRecord = False } x)
      else fromString (constructorName y)
    where
      prod = isProduct1 (Proxy :: Proxy (f a))
      wrap builder = if scNested ctx && prod then singleton '(' <> builder <> singleton ')' else builder
  isProduct1 _ = isProduct1 (Proxy :: Proxy (f a))

instance (KnownSymbol s, ShowBuilder1 f) => ShowBuilder1 (M1 i (MetaCons s fi True) f) where
  build1 ctx y@(M1 x) =
    fromString (constructorName y) <> fromString " {" <> build1 ctx { scRecord = True } x <> fromString "}"
  isProduct1 _ = isProduct1 (Proxy :: Proxy (f a))

instance ShowBuilder1 f => ShowBuilder1 (M1 i (MetaData s1 s2 b1 b2) f) where
  build1 b (M1 x) = build1 b x
  isProduct1 _ = isProduct1 (Proxy :: Proxy (f a))

instance (KnownSymbol s, ShowBuilder1 f) => ShowBuilder1 (M1 i (MetaSel (Just s) su ss ds) f) where
  build1 b (M1 x) = fromString fieldName <> fromString " = " <> build1 b x
    where
      fieldName = symbolVal (Proxy :: Proxy s)
  isProduct1 _ = isProduct1 (Proxy :: Proxy (f a))

instance ShowBuilder1 f => ShowBuilder1 (M1 i (MetaSel Nothing su ss ds) f) where
  build1 b (M1 x) = build1 b x
  isProduct1 _ = isProduct1 (Proxy :: Proxy (f a))

instance (ShowBuilder1 f, ShowBuilder1 g) => ShowBuilder1 (f :+: g) where
  build1 b (L1 x) = build1 b x
  build1 b (R1 x) = build1 b x
  isProduct1 _ = isProduct1 (Proxy :: Proxy (f a)) || isProduct1 (Proxy :: Proxy (g a))

instance (ShowBuilder1 f, ShowBuilder1 g) => ShowBuilder1 (f :*: g) where
  build1 ctx (x :*: y) = build1 ctx x <> sep <> build1 ctx y
    where
      sep = if scRecord ctx then fromString ", " else singleton ' '
  isProduct1 _ = True
