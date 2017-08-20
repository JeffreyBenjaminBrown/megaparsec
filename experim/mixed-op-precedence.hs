-- Shows that all prefix and postfix ops bind before any binary ops, regardless of precedence

import Control.Applicative (empty)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Minor but critical
sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- == Mixed ops
mixedOps :: [[Operator Parser Float]]
  -- each [Op] is a list of ops of equal precedence
  -- ops in the first list bind first
mixedOps =
  [ [ Prefix $ lexeme (C.string "a") *> pure (+1)
    , InfixL $ lexeme (C.string "b") *> pure (+)
  ],[ Prefix $ lexeme (C.string "c") *> pure (*2)
    , InfixL $ lexeme (C.string "d") *> pure (*)
  ] ]

mixedExpr :: Parser Float
mixedExpr = makeExprParser (lexeme L.float) mixedOps

-- everything works
tests :: IO ()
tests = mapM_ (putStrLn . show)
  $ map (parseMaybe $ sc *> mixedExpr)
        [ "a11"
        , "c11"
        , "c 11"
        , "c11da11"
        , "c11d a11"
        , "c11b a11"
        , " a11"
        , "c11 d a11"
        , "c11 da11"
        , "a11 da11"
        ]
