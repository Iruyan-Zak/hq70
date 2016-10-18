main = print solve

solve =
    let scoreOf n = (countCombinationOfDoubleStrike n) * fact (9-n)
    in sum $ map scoreOf [0..4]

countCombinationOfDoubleStrike :: Int -> Int
countCombinationOfDoubleStrike n =
    let initialPanelBorderStatus = replicate 8 False
        initialPanelBorderIndex = 7
    in countCombinationOfDoubleStrike' n initialPanelBorderIndex initialPanelBorderStatus

countCombinationOfDoubleStrike' :: Int      -- Rest count of double strike
                                -> Int      -- Current index of panel borders
                                -> [Bool]   -- Current status of panel borders
                                -> Int      -- Return: Number of double strike patterns
countCombinationOfDoubleStrike' 0 _ (True:True:_) = 0
countCombinationOfDoubleStrike' 0 _ _ = 1
countCombinationOfDoubleStrike' _ index _ | index < 0 = 0
countCombinationOfDoubleStrike' n index status =
        let mask' = take 8 $ drop index mask
            status' = zipWith (||) status mask'
        in (countCombinationOfDoubleStrike' (n-1) (index-2) status') + (countCombinationOfDoubleStrike' n (index-1) status)

mask = cycle $ True : (replicate 7 False)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)
