-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

-- What is the sum of the digits of the number 21000?

listOfDigits :: Integer -> [Integer]
listOfDigits x
  | abs(x) `elem` [0..9] = [x]
  | otherwise      = (x `mod` 10) : (listOfDigits (x `div` 10 ))

main = putStrLn $ show . sum $ listOfDigits $ 2 ^ 1000
