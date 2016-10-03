#!/usr/bin/env stack
-- stack runghc

paskal2014 :: Int
paskal2014 =  2014

length.findIndices


paskalRow :: Int -> [Int]
paskalRow 1 =[1]
paskalRow n =  zipWith xOr (pushFront$paskalRow $n-1) (reverse $ pushFront$paskalRow $n-1)

pushFront::[Int] -> [Int]
pushFront = (:) 0

xOr :: Int -> Int -> Int
xOr a b = (a-b)*(a-b)

main :: IO ()
main = do
	print $ paskal2014