main = print $ solve 0 3 [True, True]

solve :: Int -> Int -> [Bool] -> Int
solve f_count steers l =
    let tp = zip (init l) (tail l)
        next_l = True : (map (\(a, b) -> a /= b) tp) ++ [True]
        new_f_count = f_count + (length . filter (\b -> not b)) next_l
    in  if new_f_count >= 2014 then steers
        else solve new_f_count (steers + 1) next_l
