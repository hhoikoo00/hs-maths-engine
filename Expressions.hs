{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
module Expressions where

import Data.Maybe

---------------------------------------------------------------------------
-- Type classes and class instances

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
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
  signum  = undefined
  abs     = undefined

instance Fractional Exp where
  fromRational  = Val . realToFrac
  -- Optimizations for (/)
  (/) 0 _   = 0
  (/) e 1   = e
  (/) e e'  = BinApp Div e e'
  -- Functions below unimplemented
  recip = undefined

instance Floating Exp where
  sin     = UnApp Sin
  cos     = UnApp Cos
  log     = UnApp Log
  -- Functions below unimplemented
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
-- Displaying Expressions

instance Show UnOp where
  show Neg  = "-"
  show Sin  = "sin"
  show Cos  = "cos"
  show Log  = "log"

instance Show BinOp where
  show Add  = "+"
  show Mul  = "*"
  show Div  = "/"

instance Show Exp where
  show (Val e)  = show e
  show (Id e)   = e
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
              , (Log, log)
              ]

binOpFunc :: (Num a, Fractional a, Floating a) => BinOp -> (a -> a -> a)
binOpFunc op
  = lookUp op table
    where
      table = [ (Add, (+))
              , (Mul, (*))
              , (Div, (/))
              ]

eval :: Exp -> Env -> Double
eval (Val v) _
  = v
eval (Id x) env
  = lookUp x env
eval (UnApp op e) env
  = (unOpFunc op) (eval e env)
eval (BinApp op e e') env
  = (binOpFunc op) (eval e env) (eval e' env)
