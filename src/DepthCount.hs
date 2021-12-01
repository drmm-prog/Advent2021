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

main = do
    print(mdc [1,2,3,4])
    print(mdc [6,2,5,2,1])
    print(mdc [1.0,9.5,9.4,10.1,3])
    print(mdc [1])
    print(mdc ([]::[Int]))
