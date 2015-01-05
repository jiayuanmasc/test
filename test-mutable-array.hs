import qualified Data.List as L
import Data.Array.IO
import Control.Monad

readInput :: IO [Int]
readInput = do
	n <- getLine
	acc <- getLine
	let parseDigit x = if x == "0" then 10 else read x
	return (map parseDigit $ L.words acc)


solve :: [Int] -> IO Int
solve acc =
	do
		arr <- newArray (0,n-1) 0 :: IO (IOUArray Int Int)
		forM_ [1..n] (dp arr)
		ival <- readArray arr 0
		sol <- foldM (minV arr) ival [1..(n-1)]
		return (sol + n)
	where
		n = length acc
		diff a b = abs ((acc!!(a-1)) - (acc!!(b-1)))
		minV arr val i = do
			v <- readArray arr i
			return $ min val v
		minM arr i val j = do
			v <- readArray arr j
			return $ min val (v + diff i j)
		dp arr i = forM_ [(i-1),(i-2)..0] $
			(\j -> 
				if (j /= (i-1))
				then do
					val <- readArray arr j
					writeArray arr j (val + diff (i) (i-1))
				else do
					val <- readArray arr 0
					minval <- foldM (minM arr i) val [1..(i-2)]
					writeArray arr j minval
			)
			

main :: IO ()
main = do
	input <- readInput
	ans <- solve input
	putStrLn $ show ans