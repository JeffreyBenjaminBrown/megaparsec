-- | Handles optional bracketing terms. (I need this for the Hash language[1],
-- in which the # operator can be used as infix, prefix or postfix.)
-- [1] https://github.com/JeffreyBenjaminBrown/digraphs-with-text/tree/master/Hash

module Dwt.Parse where

import Control.Applicative (empty)
import Data.Void (Void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

test :: IO ()
test = mapM_ (putStrLn . show) $ map (parseMaybe p)
  [ "s 1 t"
  , "s 1"
  , "1 t"
  ]

p :: Parser [String]
p = do
  sc
  x <- (:[]) <$> option "" (C.string "s") <* sc
  y <- (:[]) . show <$> (L.float :: Parser Float) <* sc
  z <- (:[]) <$> option "" (C.string "t") <* sc
  return $ x ++ y ++ z

sc :: Parser ()
sc = L.space C.space1 empty empty
