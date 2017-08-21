-- | In Text.Megaparsec.Expr, all prefix and postfix ops bind
-- before any binary ops, regardless of precedence.

import Control.Applicative (empty)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

tests :: IO ()
tests = mapM_ (putStrLn . show)
  $ map (parseMaybe $ sc *> expr)
  [ "2*1"
  , "p(2+1)"
  , "q (2+1)"
  , "q1"
  ]

expr :: Parser Float
expr = makeExprParser term ops

term :: Parser Float
term = parens expr <|> (lexeme L.float)

ops :: [[Operator Parser Float]]
  -- each [Op] is a list of ops of equal precedence
  -- ops in the first list bind first
ops =
  -- the prefixes here must abut, I hope
  [ [ Prefix $ (C.string "p") *> pure (+1)
    , InfixL $ lexeme (C.string "+") *> pure (+)
  ],[ Prefix $ (C.string "q") *> pure (*2)
    , InfixL $ lexeme (C.string "*") *> pure (*)
  ] ]

-- little things
sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String -- is a lexeme; consumes trailing space
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
