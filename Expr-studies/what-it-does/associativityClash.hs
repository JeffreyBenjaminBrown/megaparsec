-- | In Text.Megaparsec.Expr, operators of equal precedence with different
-- associativities cannot (in serial, without parentheses) be mixed.

import Control.Applicative (empty)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

test :: IO ()
test = mapM_ (putStrLn . show)
  $ map (parseMaybe $ sc *> assocExpr)
        [ "1.0  L  2.0  L  3.0"
        , "1.0  R  2.0  R  3.0"
        , "1.0  N  2.0  N  3.0"
        , "1.0  R  2.0  L  3.0"
        , "1.0  L  2.0  R  3.0"
        , "1.0  N  2.0  L  3.0"
        , "1.0  L  2.0  N  3.0"
        , "1.0  N  2.0  R  3.0"
        , "1.0  R  2.0  N  3.0"
        ]

assocExpr :: Parser Float
assocExpr = makeExprParser (lexeme L.float) assocOps

-- Associative ops
assocOps :: [[Operator Parser Float]]
  -- each [Op] is a list of ops of equal precedence
  -- ops in the first list bind first
  -- here there is only one list, so they all bind equally fast
assocOps =
  [ [ InfixR $ lexeme (C.string "R") *> pure (+)
    , InfixL $ lexeme (C.string "L") *> pure (+)
    , InfixN $ lexeme (C.string "N") *> pure (+)
  ] ]

-- little things
sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
