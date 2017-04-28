-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

divisors = map (flip(mod)) [11..20]
cond x = map ((==0) . ($x)) divisors
solutions = [x | x <- [2520, 5040..], and $ cond x]
main = putStrLn $ show $ head solutions
