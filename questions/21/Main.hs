main :: IO()
main = print $ solve 2014 [True]

solve :: Int -> [Bool] -> Int
solve rest_count line =
    let nextline = next_line line
        new_rest_count = rest_count - (length . filter not) nextline
    in  if new_rest_count > 0 then solve new_rest_count nextline
        else length nextline

next_line :: [Bool] -> [Bool]
next_line line =
    let children = zipWith (/=) (init line) (tail line)
    in True : children ++ [True]
