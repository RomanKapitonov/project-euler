-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, 
-- there are exactly 6 routes to the bottom right corner.

-- How many such routes are there through a 20×20 grid?

factorial :: Integral a => a -> a
factorial n = product [1..n]

choose :: Integral a => a -> a -> a
n `choose` k
  | k < 0 = 0
  | k > n = 0
  | otherwise = factorial n `div` (factorial k * factorial (n - k))

main = putStrLn $ show $ choose 40 20