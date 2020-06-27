{-# LANGUAGE ExistentialQuantification #-}
module IC.TestSuite where

import Control.Exception
import Control.Monad
import Data.List


data TestCase = forall a b . (Reformat a, Show b)
              => TestCase String (a -> b) [(a, b)] (b -> b -> Bool)

testCase s f c = TestCase s f c (==)

floatTestCase s f c = TestCase s f c weakEquals
  where
    weakEquals a b = (abs (a - b)) < epsilon
    epsilon = 0.00001

goTest (TestCase name f cases eq) = do
  counts <- forM cases (handle majorExceptionHandler . goTestOne name f eq)
  let passes = filter id counts
  putStrLn $ name ++ ": " ++ show (length passes)
                  ++ " / " ++ show (length counts)
  where
    majorExceptionHandler :: SomeException -> IO Bool
    majorExceptionHandler e = putStrLn ("Argument exception: " ++ show e) >> return False

goTestOne name f eq (input, expected) = handle exceptionHandler $ do
  r <- evaluate (f input)
  if r `eq` expected
    then return True
    else failedStanza False r
  where
    failedStanza :: Show x => Bool -> x -> IO Bool
    failedStanza b x = do
      putStr . unlines $ [ " > " ++ name ++ " " ++ reformat input ++ " = " ++
                            (if b then "Exception: " else "") ++ show x
                         , "   test case expected: " ++ show expected
                         , ""
                         ]
      return False

    exceptionHandler :: SomeException -> IO Bool
    exceptionHandler = failedStanza True

class Reformat a where
  reformat :: a -> String

(==>) = (,)

mkId :: (a, b) -> (Id a, b)
mkId (x,y) = (Id x, y)

uncurry4 f (a, b, c, d) = f a b c d

pShow :: Show a => a -> String
pShow x = showParen True (shows x) ""

data Id a = Id { unId :: a }

instance Show a => Reformat (Id a) where
  reformat = pShow . unId

instance (Show a, Show b, Show c, Show d) => Reformat (a,b,c,d) where
  reformat (a,b,c,d) = unwords [pShow a, pShow b, pShow c, pShow d]

uncurry3 f (a, b, c) = f a b c

instance (Show a, Show b, Show c) => Reformat (a,b,c) where
  reformat (a,b,c) = unwords [pShow a, pShow b, pShow c]

instance Reformat Char where
  reformat c = show c

instance (Show a, Show b) => Reformat (a,b) where
  reformat (a,b) = unwords [pShow a, pShow b]

instance Reformat Int where
  reformat x = show x

instance Show a => Reformat [a] where
  reformat = show

