import System.IO

dc :: (Ord a) => [a] -> Int
dc xs = idc xs 0 where
    idc [] n = n
    idc [x] n = n
    idc (x1:(x2:xs)) n = if x2 > x1 then idc (x2:xs) (n+1) else idc (x2:xs) n

mdc :: (Ord a) => [a] -> Maybe Int
mdc xs = imdc xs Nothing where
    imdc [] _ = Nothing
    imdc [x] m = m
    imdc (x1:(x2:xs)) Nothing = if x2 > x1 then imdc (x2:xs) (Just 1) else imdc (x2:xs) (Just 0)
    imdc (x1:(x2:xs)) (Just n) = if x2 > x1 then imdc (x2:xs) (Just (n+1)) else imdc (x2:xs) (Just n)

tc :: (Num a) => [a] -> [a]
tc [] = []
tc (x1:[]) = []
tc (x1:x2:[]) = []
tc (x1:x2:x3:xs) = (x1+x2+x3) : tc (x2:x3:xs)

main = do
    handle <- openFile "depth_data.txt" ReadMode
    contents <- hGetContents handle
    let depths = lines contents
    print(mdc ((map read depths)::[Int]))
    let ds = (map read depths)::[Int]
    print(mdc (tc ds))
