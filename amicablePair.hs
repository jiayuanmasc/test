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
solve n = filter isAmicable
  where
    cand = [ (x,y) | x<-[1..n], y<-[1..n], x > y ]

main :: IO ()
main = do
  n <- read <$> getLine
  putStrLn . show $ solve n
