import qualified Data.List as L
import Data.Char
import Data.Array

readInput :: IO [Int]
readInput = do
  n <- getLine
  acc <- getLine
  let parseDigit x = if x == "0" then 10 else read x
  return (map parseDigit $ L.words acc)

solve :: [Int] -> Int
solve acc = n + minimum [ (table!(n,i)) | i <- [0..(n-1)] ]
  where
    n = length acc
    diff a b = abs ((acc!!(a-1)) - (acc!!(b-1)))
    table = array ((0,0), (n,n-1)) [ (idx, dp idx) | idx <- [ (i,j) | i <- [1..n], j <- [0..(i-1)] ] ]
    dp (1,0) = 0
    dp (i,j)
      | j /= i-1 = (table!(i-1,j)) + diff i (i-1)
      | otherwise = minimum $ (table!(i-1,0)) : [ (table!(i-1,k)) + (diff i k) | k <- [1..(i-2)] ] 

main :: IO ()
main = do
  input <- readInput
  putStrLn $ show $ solve input