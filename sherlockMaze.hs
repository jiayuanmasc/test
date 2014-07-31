import Data.Array
import Control.Monad

countPath :: Int -> Int -> Int -> Int
countPath n' m' k' = if (n' == 1) && (m' == 1) && (k' == 0)
	then 1
	else foldr (\xx yy -> rem (xx+yy) md) 0 [(table ! (n',m',x,1) + table ! (n',m',x,0)) `rem` md | x <- [0..k']]
	where
		table = array ((1,1,0,0), (n',m',k',1)) [ (idx, dp idx) | idx <- range((1,1,0,0), (n',m',k',1)) ]
		dp (1,1,_,_) = 0
		dp (_,1,0,0) = 1
		dp (1,_,0,1) = 1
		dp (i,j,k,0) = if k > 0 && i > 1
			then (table ! (i-1,j,k,0) + table ! (i-1,j,k-1,1)) `rem` md
			else if i > 1 then table ! (i-1,j,k,0) else 0
		dp (i,j,k,1) = if k > 0 && j > 1
			then (table ! (i,j-1,k,1) + table ! (i,j-1,k-1,0)) `rem` md
			else if j > 1 then table ! (i,j-1,k,1) else 0
		dp _ = 0
		md = 1000000007


main :: IO ()
main = do
	t <- getLine
	forM_ [1..(read t)] $ 
		(\_ -> do
		input <- getLine
		let prob = map read $ words input
		putStrLn $ show $ countPath (prob !! 0) (prob !! 1) (prob !! 2))
