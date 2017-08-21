-- | Demonstrates how a level of left-infix operators
-- is parsed in Text.Megaparsec.Expr.

import Control.Applicative (empty)
import Data.Void (Void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

test = mapM_ (putStrLn . show) $ map (parseMaybe theExprParser)
  ["3 + 2"
  , "3 * 2"
  , "3 * 2 + 4"
  , "3 + 2 * 4"
  ]

theExprParser :: Parser Float
theExprParser = addPrecLevel (lexeme L.float) [plus, times]

plus :: Num a => Parser (a -> a -> a)
plus = const (+) <$> (lexeme $ C.string "+")

times :: Num a => Parser (a -> a -> a)
times = const (*) <$> (lexeme $ C.string "*")

-- functions modified from Text.Megaparsec.Expr
addPrecLevel :: MonadParsec e s m => m a -> [m (a -> a -> a)] -> m a
addPrecLevel term leftAssocOps =
  term >>= \x -> choice [leftAssocOps' x, return x] <?> "operator"
  where leftAssocOps'  = pInfixL (choice leftAssocOps) term

pInfixL :: MonadParsec e s m => m (a -> a -> a) -> m a -> a -> m a
pInfixL op p x = do
  f <- op
  y <- p
  let r = f x y
  pInfixL op p r <|> return r

-- little things
sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
