-- | Lexemes can abut without intervening space. This surprised me
-- because @`lexeme` is defined by a space consumer. It holds in
-- Text.Megaparsec generally, not just Text.Megaparsec.Expr.

module Dwt.Parse where

import Control.Applicative (empty)
import Data.Void (Void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

demo :: IO ()
demo = parseTest f "st"
  where f = do sc
               x <- C.string "s" <* sc
               y <- C.string "t" <* sc
               return $ x ++ y

sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
