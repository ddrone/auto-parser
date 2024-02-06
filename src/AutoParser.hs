module AutoParser where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (space1)
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
  parse = space *> Lexer.decimal

instance TextParser c => TextParser1 (K1 i c) where
  parse1 = K1 <$> inner
    where
      inner = parse <|> (symbol "(" *> parse <* symbol ")")

instance (TextParser1 f, TextParser1 g) => TextParser1 (f :+: g) where
  parse1 = (L1 <$> parse1) <|> (R1 <$> parse1)

instance (TextParser1 f, TextParser1 g) => TextParser1 (f :*: g) where
  parse1 = (:*:) <$> parse1 <*> parse1
