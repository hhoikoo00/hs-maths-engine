{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
module Calculus where

import Expressions
import Algebra

diff :: Exp -> String -> Exp
diff
  = (simplify .) . diff'
    where
      diff' (Val _) _
        = 0
      diff' (Id x) var
        | x == var  = 1
        | otherwise = 0
      diff' (UnApp op e) var
        | op == Neg = negate (diff e var)
        | op == Sin = cos e * diff e var
        | op == Cos = negate (sin e * diff e var)
        | op == Tan = 1 / (cos (diff e var) ** 2)
        | op == Log = (diff e var) / e
        | otherwise = error ("diff: unsupported operator " ++ show op)
      diff' be@(BinApp op e e') var
        | op == Add = diff e var + diff e' var
        | op == Mul = (e * diff e' var) + (diff e var * e')
        | op == Div = ((e * diff e' var) + (diff e var * e')) / (e' * e')
        -- y = e ** e' => diff(y) == y * diff(e' * log(e))
        -- apply log to both sides then derive
        | op == Pow = be * (diff (e' * log e) var)
        | otherwise = error ("diff: unsupported operator " ++ show op)

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
