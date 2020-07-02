{-# OPTIONS_GHC -Wall -Wno-missing-signatures -fwarn-tabs #-}
module Tests where

import IC.TestSuite hiding (Id)

import Expressions
import Calculus

---------------------------------------------------------------------------
-- Example expressions

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

---------------------------------------------------------------------------
-- Test cases

evalTests
  = [ ((Val 7),  [("x",380)])
        ==> 7.0
    , ((Id "a"), [("x",380), ("a",42), ("t",10)])
        ==> 42.0
    , ( (BinApp Add (Val (-5)) (Id "t'")), [("t",10), ("t'",18)])
        ==> 13.0
    , ((UnApp Neg (BinApp Add (Val (-5)) (Id "t'"))), [("t",10), ("t'",19)])
        ==> (-14.0)
    , ((BinApp Mul (Id "x") (Id "x")), [("t",10), ("t'",18.6), ("x",-55)])
        ==> 3025.0
    , ((BinApp Div (Val 3) (Id "z")), [("z",7)])
        ==> 0.42857142857142855
    , ((UnApp Neg (Id "x")), [("x",0.37)])
        ==> (-0.37)
    , ((UnApp Sin (Val 2.4)), [])
        ==> 0.675463180551151
    , ((UnApp Cos (Val 2.4)), [])
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
  = [ (e1, "x") ==> Val 5.0
    , (e2, "x") ==> BinApp Add (Id "x") (Id "x")
    , (e2, "y") ==> Val 1.0
    , (e4, "x") ==> UnApp Sin (Id "x")
    , (e5, "x") ==>
        BinApp Mul (UnApp Cos (BinApp Add (Val 1.0)
                                          (UnApp Log (BinApp Mul (Val 2.0)
                                                                 (Id "x")))))
                   (BinApp Div (Val 2.0)
                               (BinApp Mul (Val 2.0)
                                           (Id "x")))
    , (e6, "x") ==>
        BinApp Div (BinApp Mul (Val 3.0)
                               (BinApp Add (Id "x")
                                           (Id "x")))
                   (BinApp Add (BinApp Mul (Val 3.0)
                                          (BinApp Mul (Id "x")
                                                      (Id "x")))
                               (Val 2.0))
    ]

maclaurinTests
  = [ (UnApp Sin (Id "x"), 2, 2) ==> 2.0
    , (UnApp Sin (Id "x"), 2, 3) ==> 2.0
    , (UnApp Sin (Id "x"), 2, 5) ==> 0.6666666666666667
    , (UnApp Sin (Id "x"), 2, 7) ==> 0.9333333333333333
    , (UnApp Sin (Id "x"), 2, 9) ==> 0.9079365079365079
    , (UnApp Cos (Id "x"), 4, 9)  ==> (-0.39682539682539764)
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
