main = print solve

mask = (replicate 7 False) ++ [True] ++ (replicate 7 False)

solve :: Int
solve =
    let doubles = replicate 8 False
        hoge' n = (fact $ 9 - n) * hoge n 7 doubles
    in sum . (map  hoge') $ [0..4]

hoge :: Int -> Int -> [Bool] -> Int
hoge 0 _ doubles
    | doubles !! 0 && doubles !! 7 = 0
    | otherwise = 1
hoge n p doubles
    | p < 0 = 0
    | otherwise =
        let mask' = take 8 $ drop p mask
            doubles' = zipWith (||) doubles mask'
        in (hoge (n-1) (p-2) doubles') + (hoge n (p-1) doubles)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)
