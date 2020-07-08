{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
module Expressions where

import Data.Maybe

---------------------------------------------------------------------------
-- Type classes and class instances

data UnOp = Neg | Sin | Cos | Tan | Log
          deriving (Eq, Ord)

data BinOp = Add | Mul | Div | Pow
           deriving (Eq, Ord)

data Exp =  Val Double
          | Id String
          | UnApp UnOp Exp
          | BinApp BinOp Exp Exp
         deriving (Eq, Ord)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Convert normal expressions to Exp type

instance Num Exp where
  fromInteger = Val . fromIntegral
  -- Optimizations for negate
  negate 0              = 0
  negate (UnApp Neg n)  = n
  negate n              = UnApp Neg n
  -- Optimizations for (+)
  (+) e 0   = e
  (+) 0 e   = e
  (+) e e'  = BinApp Add e e'
  -- Optimizations for (*)
  (*) 0 _   = 0
  (*) _ 0   = 0
  (*) e 1   = e
  (*) 1 e   = e
  (*) e e'  = BinApp Mul e e'
  -- Functions below unimplemented
  signum  = error "No signum for symbolic numbers"
  abs     = error "No abs for symbolic numbers"

instance Fractional Exp where
  fromRational  = Val . realToFrac
  -- Optimizations for (/)
  (/) 0 _   = 0
  (/) _ 0   = error "Division by zero!"
  (/) e 1   = e
  (/) e e'  = BinApp Div e e'
  -- Optimizations for recip
  recip (BinApp Div e e') = (/) e' e
  recip e                 = (/) 1 e

instance Floating Exp where
  pi  = Id "pi"
  sin = UnApp Sin
  cos = UnApp Cos
  tan = UnApp Tan
  exp = (**) (Id "e")
  -- Optimizations for (**)
  (**) 0 0  = error "0 ** 0 undefined!"
  (**) 0 _  = 0
  (**) _ 0  = 1
  (**) 1 _  = 1
  (**) e 1  = e
  (**) e p  = BinApp Pow e p
  -- Optimizations for log
  log (Id "e")                      = 1
  log (BinApp Mul (Id "e") (Val n)) = Val n
  log (BinApp Mul (Val n) (Id "e")) = Val n
  log e                             = UnApp Log e
  -- Optimizations for logBase
  logBase b v = (/) (log v) (log b)
  -- Functions below unimplemented
  sqrt    = error "sqrt not yet implemented"
  asin    = error "Inverse trigonometric function not yet implemented"
  acos    = error "Inverse trigonometric function not yet implemented"
  atan    = error "Inverse trigonometric function not yet implemented"
  sinh    = error "Hyperbolic function not yet implemented"
  cosh    = error "Hyperbolic function not yet implemented"
  tanh    = error "Hyperbolic function not yet implemented"
  asinh   = error "Hyperbolic function not yet implemented"
  acosh   = error "Hyperbolic function not yet implemented"
  atanh   = error "Hyperbolic function not yet implemented"

---------------------------------------------------------------------------
-- Displaying Expressions

instance Show UnOp where
  show Neg  = "-"
  show Sin  = "sin"
  show Cos  = "cos"
  show Tan  = "tan"
  show Log  = "log"

instance Show BinOp where
  show Add  = "+"
  show Mul  = "*"
  show Div  = "/"
  show Pow  = "**"

instance Show Exp where
  show (Val e)    = show e
  show (Id "pi")  = "π"
  show (Id e)     = e
  -- Special cases for Unary Operators
  show (UnApp Neg e)
    = show Neg ++ show e
  show (UnApp op e@(BinApp _ _ _))
    = show op ++ show e
  show (UnApp op e)
    = show op ++ "(" ++ show e ++ ")"
  -- Special cases for Binary Operators
  show (BinApp Add e (UnApp Neg e'))
    = "(" ++ show e ++ show Neg ++ show e' ++ ")"
  show (BinApp op e e')
    = "(" ++ show e ++ show op ++ show e' ++ ")"

---------------------------------------------------------------------------
-- Helper Functions

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp
  = (fromJust .) . lookup

unOpFunc :: (Num a, Fractional a, Floating a) => UnOp -> (a -> a)
unOpFunc op
  = lookUp op table
    where
      table = [ (Neg, negate)
              , (Sin, sin)
              , (Cos, cos)
              , (Tan, tan)
              , (Log, log)
              ]

binOpFunc :: (Num a, Fractional a, Floating a) => BinOp -> (a -> a -> a)
binOpFunc op
  = lookUp op table
    where
      table = [ (Add, (+))
              , (Mul, (*))
              , (Div, (/))
              , (Pow, (**))
              ]

eval :: Exp -> Env -> Double
eval (Val v) _
  = v
eval (Id "e") _
  = exp 1
eval (Id "pi") _
  = pi
eval (Id x) env
  = lookUp x env
eval (UnApp op e) env
  = (unOpFunc op) (eval e env)
eval (BinApp op e e') env
  = (binOpFunc op) (eval e env) (eval e' env)
