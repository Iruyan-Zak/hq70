CarAndSpace = (Int, Int) (Int, Int)
Request = CarAndSpace Int

solve =
    let initial = 0 : replicate 9998 ++ [0]
    

solve' :: [Request] -> [Int] -> Int
solve' ((Request (CarAndSpace (cx, cy) (sx, sy)) dist):queue) costs@(0:_)
    | (costs !! (100 * (10 * cx + cy) + 10 * sx + sy)) /= -1 =
        solve' queue costs
    | otherwise =
        
solve' _ (ans:_) = ans

setAt :: [a] -> Int -> a -> [a]
setAt list index value =
    let heads = take index list
        tails = drop (index+1) list
    in  heads ++ [value] ++ tails
