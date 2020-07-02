{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
module Calculus where

import Expressions

diff :: Exp -> String -> Exp
diff (Val _) _
  = 0
diff (Id x) var
  | x == var  = 1
  | otherwise = 0
diff (UnApp op e) var
  | op == Neg = negate (diff e var)
  | op == Sin = cos e * diff e var
  | op == Cos = negate (sin e * diff e var)
  | op == Log = (diff e var) / e
  | otherwise = error "diff: unsupported unary operator"
diff (BinApp op e e') var
  | op == Add = diff e var + diff e' var
  | op == Mul = (e * diff e' var) + (diff e var * e')
  | op == Div = ((e * diff e' var) + (diff e var * e')) / (e' * e')
  | otherwise = error "diff: unsupported binary operator"

maclaurin :: Exp -> Double -> Int -> Double
maclaurin
  = taylor 0.0

taylor :: Double -> Exp -> Double -> Int -> Double
taylor centre e val n
  = sum (take n (zipWith3 calcTerm dxs xs facts))
    where
      var   = "x"
      dxs   = map (flip eval [(var, centre)]) (iterate (flip diff var) e)
      xs    = iterate (*(val - centre)) 1
      facts = 1 : scanl1 (*) [1..]
      calcTerm dx x fact
        = (dx * x) / fact
