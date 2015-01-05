import qualified Data.List as L

amicable :: (Int, Int) -> Bool
amicable (a, b) = sumOfFactor a == sumOfFactor b
	where
		sumOfFactor x = 1 + sum [ if f == quot x f then f else (f + quot x f) | f <- [2..floor (sqrt (fromIntegral x))], mod x f == 0 ]

solve :: Int -> (Int, Int)
solve n = head $ L.dropWhile (not . amicable) [ (x,y) | x<-[n,(n-1)..1], y<-[(x-1),(x-2)..1] ]