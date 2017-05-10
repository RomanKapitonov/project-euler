-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

-- Find the sum of all the primes below two million.

import Data.List

primes = 2:filter isPrime [3,5..]
isPrime = null . tail . primeFactors
primeFactors n = factor n primes
    where
        factor n l@(p:ps)
            | p * p > n    = [n]
            | mod n p == 0 = p:factor (div n p) l
            | otherwise    = factor n ps

solution = foldl1' (+) (takeWhile (<2000000) primes)
