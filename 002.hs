-- Each new term in the Fibonacci sequence is generated by adding the previous two terms.
-- By starting with 1 and 2, the first 10 terms will be:

-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

-- By considering the terms in the Fibonacci sequence whose values do not exceed four million,
-- find the sum of the even-valued terms.

fibonacci :: [Int]
fibonacci = [x | x <- zipWith (+) (1:fibonacci) (0:1:fibonacci)]

fibonacci' :: [Int]
fibonacci' = do
  x <- zipWith (+) (1:fibonacci') (0:1:fibonacci')
  return x

fibonacci'' :: [Int]
fibonacci'' = 1 : 1 : [a + b | (a, b) <- zip fibonacci'' (tail fibonacci'')]

sum $ filter even $ takeWhile (<4000000) fibonacci'
