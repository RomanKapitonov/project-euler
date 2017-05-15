-- n! means n × (n − 1) × ... × 3 × 2 × 1

-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

-- Find the sum of the digits in the number 100!

factorial :: Integral a => a -> a
factorial n = product [1..n]

listOfDigits :: Integer -> [Integer]
listOfDigits x
  | abs(x) `elem` [0..9] = [x]
  | otherwise      = (x `mod` 10) : (listOfDigits (x `div` 10 ))

main = putStrLn $ show $ sum $ listOfDigits $ factorial 100
