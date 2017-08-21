-- | A scheme for handling optional arguments to an infix operator.

module Dwt.Parse where

import Control.Applicative (empty)
import Data.Void (Void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space C.space1 empty empty

p :: Parser [String]
p = do
  sc
  x <- (:[]) <$> option "" (C.string "s") <* sc
  y <- (:[]) . show <$> (L.float :: Parser Float) <* sc
  z <- (:[]) <$> option "" (C.string "t") <* sc
  return $ x ++ y ++ z

demo :: IO ()
demo = mapM_ (putStrLn . show)
  $ map (parseMaybe p) ["s 1 t", "s 1", "1 t"]
