module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  fromInteger = undefined
  negate      = undefined
  (+)         = undefined
  (*)         = undefined
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined

instance Fractional Exp where
  fromRational = undefined
  (/)          = undefined
-- Leave the following one undefined...
  recip        = undefined

instance Floating Exp where
  sin     = undefined
  cos     = undefined
  log     = undefined
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp
  = undefined

showExp :: Exp -> String
showExp
  = undefined

eval :: Exp -> Env -> Double
eval
  = undefined

diff :: Exp -> String -> Exp
diff
  = undefined

maclaurin :: Exp -> Double -> Int -> Double
maclaurin
  = undefined

---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
