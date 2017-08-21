-- | Demonstrates how layers of operator precedence are accumulated
-- in Text.Megaparsec.Expr

import Text.Megaparsec

import Control.Applicative (empty)
import Data.Void (Void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

test :: IO ()
test = mapM_ (putStrLn . show) $ map (parseMaybe theExprParser)
  [""
  , "3"
  , "3 + 2"
  , "3 * 2"
  , "3 * 2 + 4"
  , "4 + 2 * 3"
  ]

theExprParser :: Parser Float
theExprParser = foldl addPrecLevel (lexeme L.float) [times, plus]

plus :: Num a => Parser (a -> a -> a)
plus = const (+) <$> (lexeme $ C.string "+")

times :: Num a => Parser (a -> a -> a)
times = const (*) <$> (lexeme $ C.string "*")

-- simplified from Text.Megaparsec.Expr
addPrecLevel :: MonadParsec e s m => m a -> m (a -> a -> a) -> m a
addPrecLevel term op =
  term >>= \x -> choice [pInfixL op term x, return x] <?> "operator"

-- unchanged from Text.Megaparsec.Expr
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
  
