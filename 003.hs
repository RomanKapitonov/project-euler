-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

primes = 2:filter isPrime [3,5..]
isPrime = null . tail . primeFactors

primeFactors n = factor n primes
  where
    factor n l@(p:ps)
      | p * p > n    = [n]
      | mod n p == 0 = p:factor (div n p) l
      | otherwise    = factor n ps

main = putStr $ show $ maximum $ primeFactors 600851475143