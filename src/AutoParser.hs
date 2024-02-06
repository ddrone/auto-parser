module AutoParser where

import Control.Applicative ((<|>))
import Data.Proxy
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Megaparsec (Parsec, sepBy1)
import Text.Megaparsec.Char (space1)
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

space :: Parser ()
space = Lexer.space space1 (Lexer.skipLineComment "--") (Lexer.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

class TextParser a where
  parse :: Parser a
  default parse :: (Generic a, TextParser1 (Rep a)) => Parser a
  parse = to <$> parse1

class TextParser1 f where
  parse1 :: Parser (f p)

instance TextParser Int where
  parse = Lexer.decimal <* space

instance TextParser a => TextParser [a] where
  parse = symbol "[" *> sepBy1 parse (symbol ",") <* symbol "]"

instance TextParser1 U1 where
  parse1 = pure U1

instance TextParser c => TextParser1 (K1 i c) where
  parse1 = K1 <$> inner
    where
      inner = parse <|> (symbol "(" *> parse <* symbol ")")

instance (KnownSymbol s, TextParser1 f) => TextParser1 (M1 i (MetaCons s fi b) f) where
  parse1 = symbol (Text.pack constructorName) *> (M1 <$> parse1)
    where
      constructorName = symbolVal (Proxy :: Proxy s)

instance TextParser1 f => TextParser1 (M1 i (MetaData s1 s2 b1 b2) f) where
  parse1 = M1 <$> parse1

instance TextParser1 f => TextParser1 (M1 i (MetaSel ms su ss ds) f) where
  parse1 = M1 <$> parse1

instance (TextParser1 f, TextParser1 g) => TextParser1 (f :+: g) where
  parse1 = (L1 <$> parse1) <|> (R1 <$> parse1)

instance (TextParser1 f, TextParser1 g) => TextParser1 (f :*: g) where
  parse1 = (:*:) <$> parse1 <*> parse1
