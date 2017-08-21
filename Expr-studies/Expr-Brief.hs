-- a copy of Text.Megaparsec.Expr without comments, for easier study

module Text.Megaparsec.Expr
-- jbb edit
--  ( Operator (..)
--  , makeExprParser )
where

import Text.Megaparsec

data Operator m a
  = InfixN  (m (a -> a -> a)) -- ^ Non-associative infix
  | InfixL  (m (a -> a -> a)) -- ^ Left-associative infix
  | InfixR  (m (a -> a -> a)) -- ^ Right-associative infix
  | Prefix  (m (a -> a))      -- ^ Prefix
  | Postfix (m (a -> a))      -- ^ Postfix

makeExprParser :: MonadParsec e s m
  => m a               -- ^ Term parser
  -> [[Operator m a]]  -- ^ Operator table, see 'Operator'
  -> m a               -- ^ Resulting expression parser
makeExprParser = foldl addPrecLevel
{-# INLINEABLE makeExprParser #-}

addPrecLevel :: MonadParsec e s m => m a -> [Operator m a] -> m a
addPrecLevel term ops =
  term' >>= \x -> choice [ras' x, las' x, nas' x, return x] <?> "operator"
  where (ras, las, nas, prefix, postfix) = foldr splitOp ([],[],[],[],[]) ops
        term' = pTerm (choice prefix) term (choice postfix)
        ras'  = pInfixR (choice ras) term'
        las'  = pInfixL (choice las) term'
        nas'  = pInfixN (choice nas) term'

pTerm :: MonadParsec e s m => m (a -> a) -> m a -> m (a -> a) -> m a
pTerm prefix term postfix = do
  pre  <- option id (hidden prefix)
  x    <- term
  post <- option id (hidden postfix)
  return . post . pre $ x

pInfixN :: MonadParsec e s m => m (a -> a -> a) -> m a -> a -> m a
pInfixN op p x = do
  f <- op
  y <- p
  return $ f x y

pInfixL :: MonadParsec e s m => m (a -> a -> a) -> m a -> a -> m a
pInfixL op p x = do
  f <- op
  y <- p
  let r = f x y
  pInfixL op p r <|> return r

pInfixR :: MonadParsec e s m => m (a -> a -> a) -> m a -> a -> m a
pInfixR op p x = do
  f <- op
  y <- p >>= \r -> pInfixR op p r <|> return r
  return $ f x y

type Batch m a =
  ( [m (a -> a -> a)]
  , [m (a -> a -> a)]
  , [m (a -> a -> a)]
  , [m (a -> a)]
  , [m (a -> a)] )

splitOp :: Operator m a -> Batch m a -> Batch m a
splitOp (InfixR  op) (r, l, n, pre, post) = (op:r, l, n, pre, post)
splitOp (InfixL  op) (r, l, n, pre, post) = (r, op:l, n, pre, post)
splitOp (InfixN  op) (r, l, n, pre, post) = (r, l, op:n, pre, post)
splitOp (Prefix  op) (r, l, n, pre, post) = (r, l, n, op:pre, post)
splitOp (Postfix op) (r, l, n, pre, post) = (r, l, n, pre, op:post)
