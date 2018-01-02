module P024 where
import Data.List

main :: IO ()
main = print $ (sort $ permutations "0123456789") !! 999999
