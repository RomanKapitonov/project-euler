-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

-- What is the 10 001st prime number?

primes = 2:filter isPrime [3,5..]
isPrime = null . tail . primeFactors
primeFactors n = factor n primes
    where
        factor n l@(p:ps)
            | p * p > n    = [n]
            | mod n p == 0 = p:factor (div n p) l
            | otherwise    = factor n ps

solution = primes !! 10000
