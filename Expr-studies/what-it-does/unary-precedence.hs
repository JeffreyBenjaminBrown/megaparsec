-- | In Text.Megaparsec.Expr, higher-precedence prefix and postfix
-- ops must be closer to the term they modify.

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

testUnaryExpr :: IO ()
testUnaryExpr = mapM_ (putStrLn . show)
  $ map (parseMaybe unaryExpr)
        [ "a3wz"  -- works
        , "a3zw"
        , "ab3"
        , "ac3"
        , "ca3"   -- works
        , "c3"   -- works
        , "a3"   -- works
        , "ca3zw"
        , "ca3wz" -- works
        ] -- the others should fail, and do

unaryExpr :: Parser Float
unaryExpr = makeExprParser L.float unaryOps

unaryOps :: [[Operator Parser Float]]
unaryOps =
  [ [ Prefix $ C.string "a" *> pure (*1)
    , Prefix $ C.string "b" *> pure (+2)
    , Postfix $ C.string "w" *> pure (/3)
    , Postfix $ C.string "x" *> pure (+(-4))
  ],[ Prefix $ C.string "c" *> pure (*4)
    , Prefix $ C.string "d" *> pure (+3)
    , Postfix $ C.string "y" *> pure (/2)
    , Postfix $ C.string "z" *> pure (+(-1))
  ] ]
