data CarAndSpace = CarAndSpace {car :: (Int, Int), space :: (Int, Int)} deriving (Show)
data Request = Request {carAndSpace :: CarAndSpace, cost :: Int} deriving (Show)

main :: IO()
main = print solve

solve :: Int
solve =
    let initialRequests = [Request (CarAndSpace (9, 9) (9, 8)) 0] -- 最終状態から逆順に探索
        initialCosts = replicate 10000 (-1)
    in  solve' initialRequests initialCosts

solve' :: [Request] -> [Int] -> Int
solve' ((Request (CarAndSpace (0, 0) (9, 9)) cost):_) _ = cost -- 初期状態（答え）を探索するとき
solve' [] _ = -1 -- 答えに到達不能(探索キューが空)
solve' (request:queue) costs
    | (costs !! index) /= -1 = solve' queue costs -- すでに探索したマスを探索するとき
    | otherwise = solve' queue' costs' -- 未探索のマスを探索するとき
    where   index = calcIndex . carAndSpace $ request
            costs' = setAt costs index $ cost request
            queue' = queue ++ (newRequests request)

calcIndex :: CarAndSpace -> Int
calcIndex (CarAndSpace (cx, cy) (sx, sy)) = 1000 * cx + 100 * cy + 10 * sx + sy

newRequests :: Request -> [Request]
newRequests (Request (carAndSpace@(CarAndSpace (cx, cy) (sx, sy))) cost) = requests
    where   requests = map (newRequest . swapCar) $ filter inMap newCarAndSpaces
            newCarAndSpaces  =
                [carAndSpace {space = (sx + 1, sy)}
                ,carAndSpace {space = (sx, sy + 1)}
                ,carAndSpace {space = (sx - 1, sy)}
                ,carAndSpace {space = (sx, sy - 1)}
                ]
            swapCar (cas@(CarAndSpace a b))
                | a == b = (CarAndSpace (sx, sy) a)
                | otherwise = cas
            newRequest cas = Request cas (cost + 1)

inMap :: CarAndSpace -> Bool
inMap = do
    sxIsOK <- inMap' . fst . space
    syIsOK <- inMap' . snd . space
    return (sxIsOK && syIsOK)

inMap' :: Int -> Bool
inMap' n = isConstrained n 0 9

isConstrained :: Int -> Int -> Int -> Bool
isConstrained value min_inclusive max_inclusive =
    min_inclusive <= value && value <= max_inclusive

setAt :: [a] -> Int -> a -> [a]
setAt list index value =
    let heads = take index list
        tails = drop (index+1) list
    in  heads ++ [value] ++ tails
