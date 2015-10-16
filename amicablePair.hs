import Control.Applicative
import Data.List

sumf :: Int -> Int
sumf n = sum $ map (dfor n) (cand n)
  where
    cand n = takeWhile (\x -> x ^ 2 <= n) [1..]
    dfor n x
      | n `mod` x /= 0 = 0
      | x == 1         = 1
      | x * x == n     = x
      | otherwise      = x + (n `div` x)

isAmicable :: (Int, Int) -> Bool
isAmicable (a, b) = (sumf a == b) && (sumf b == a) 

solve :: Int -> [(Int, Int)]
solve n = [ (k1, k3) | (k1, k2) <- cand, (k3, k4) <- cand,
                       k1 < k3, k1 == k4, k2 == k3 ]
  where
    cand = map (\x -> (x, sumf x)) [1..n]

main :: IO ()
main = do
  n <- read <$> getLine
  putStrLn . show $ solve n
