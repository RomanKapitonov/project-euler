-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

import Data.Matrix

-- An attempt to find such triple using matrices
-- (Pythagorean triples by use of matrices and linear transformations)
--
startingVector :: Matrix Integer
startingVector = Data.Matrix.fromLists [[3], [4], [5]]

matrixA :: Matrix Integer
matrixA = Data.Matrix.fromLists [
  [1, -2, 2],
  [2, -1, 2],
  [2, -2, 3]]

matrixB :: Matrix Integer
matrixB = Data.Matrix.fromLists [
  [1, 2, 2],
  [2, 1, 2],
  [2, 2, 3]]

matrixC :: Matrix Integer
matrixC = Data.Matrix.fromLists [
  [-1, 2, 2],
  [-2, 1, 2],
  [-2, 2, 3]]

-- This is not going to work since first triple with sum 1000 does not contain coprimes
--
triples :: Integer -> Matrix Integer -> [(Integer, Integer, Integer)]
triples maxValue current
  | maxValue < getElem 3 1 current = []
  | otherwise = [(a, b, c)] ++ triplesA ++ triplesB ++ triplesC
    where a = min (getElem 1 1 current) (getElem 2 1 current)
          b = max (getElem 1 1 current) (getElem 2 1 current)
          c = getElem 3 1 current
          triplesA = (triples maxValue (matrixA `multStd` current))
          triplesB = (triples maxValue (matrixB `multStd` current))
          triplesC = (triples maxValue (matrixC `multStd` current))

-- Simple solution using list comprehension
solution = head [a * b * c | a <- [1..1000], b <- [1..1000 - a], c <- [1000 - a - b], a^2 + b^2 == c^2]
