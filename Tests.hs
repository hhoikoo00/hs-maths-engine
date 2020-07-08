{-# OPTIONS_GHC -Wall -Wno-missing-signatures -fwarn-tabs #-}
module Tests where

import IC.TestSuite hiding (Id)

import Expressions
import Calculus

---------------------------------------------------------------------------
-- Example expressions

x, y, z :: Exp
x = Id "x"
y = Id "y"
z = Id "z"

e1, e2, e3, e4, e5, e6 :: Exp
e1 = 5.0 * x
e2 = x * x + y - 7
e3 = x - (y * y) / (4 * x * y - (y * y))
e4 = -cos(x)
e5 = sin(1 + log(2 * x))
e6 = log(3 * (x * x) + 2)

---------------------------------------------------------------------------
-- Test cases

a, t, t' :: Exp
a = Id "a"
t = Id "t"
t' = Id "t'"

evalTests
  = [ ( (7),  [("x",380)])
        ==> 7.0
    , ( (a), [("x",380), ("a",42), ("t",10)])
        ==> 42.0
    , ( (-5 + t'), [("t",10), ("t'",18)])
        ==> 13.0
    , ( (-(-5 + t')), [("t",10), ("t'",19)])
        ==> (-14.0)
    , ( (x * x), [("t",10), ("t'",18.6), ("x",-55)])
        ==> 3025.0
    , ( (3 / z), [("z",7)])
        ==> 0.42857142857142855
    , ( (-x), [("x",0.37)])
        ==> (-0.37)
    , ( (sin(2.4)), [])
        ==> 0.675463180551151
    , ( (cos(2.4)), [])
        ==> (-0.7373937155412454)
    , ( e1, [("x",0.37)])
        ==> (1.85)
    , ( e2, [("x",0.37), ("y", 8.2)])
        ==> 1.3369
    , ( e3, [("x",0.37), ("y", 2.0)])
        ==> 4.216153846153846
    , ( e4, [("x",0.37)])
        ==> (-0.9323273456060345)
    , ( e5, [("x",0.37)])
        ==> 0.6433720724587564
    , ( e6, [("x",0.37)])
        ==> 0.8799171617597958
    ]

diffTests
  = [ (e1, "x") ==> (5.0)
    , (e2, "x") ==> (x + x)
    , (e2, "y") ==> (1.0)
    , (e4, "x") ==> (sin(x))
    , (e5, "x") ==> (cos(1.0 + log(2.0 * x)) * (2.0 / (2.0 * x)))
    , (e6, "x") ==> ((3.0 * (x + x)) / ((3.0 * (x * x)) + 2.0))
    ]

maclaurinTests
  = [ ((sin(x)), 2, 2) ==> 2.0
    , ((sin(x)), 2, 3) ==> 2.0
    , ((sin(x)), 2, 5) ==> 0.6666666666666667
    , ((sin(x)), 2, 7) ==> 0.9333333333333333
    , ((sin(x)), 2, 9) ==> 0.9079365079365079
    , ((cos(x)), 4, 9)  ==> (-0.39682539682539764)
    ]

---------------------------------------------------------------------------
-- Test suite runner

allTestCases
  = [ floatTestCase "eval"      (uncurry eval)       evalTests
    , testCase      "diff"      (uncurry diff)       diffTests
    , floatTestCase "maclaurin" (uncurry3 maclaurin) maclaurinTests
    ]

runTests = mapM_ goTest allTestCases

main = runTests
