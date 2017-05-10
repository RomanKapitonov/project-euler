{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- The Fibonacci sequence is defined by the recurrence relation:

-- Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
-- Hence the first 12 terms will be:

-- F1 = 1
-- F2 = 1
-- F3 = 2
-- F4 = 3
-- F5 = 5
-- F6 = 8
-- F7 = 13
-- F8 = 21
-- F9 = 34
-- F10 = 55
-- F11 = 89
-- F12 = 144
-- The 12th term, F12, is the first term to contain three digits.

-- What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
    (Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22)
            (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22))

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fetch $ (Matrix 1 1 1 0) ^ n
  where fetch (Matrix _ f _ _) = f

fibs = map fib [0..]

listOfDigits :: Integer -> [Integer]
listOfDigits x
  | abs(x) `elem` [0..9] = [x]
  | otherwise            = (x `mod` 10) : (listOfDigits (x `div` 10 ))

number = snd $ last $ takeWhile ((< 1000) . length . listOfDigits . fst ) $ zip fibs [1..]

main = putStr $ show number