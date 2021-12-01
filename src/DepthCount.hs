dc :: (Ord a) => [a] -> Int
dc xs = idc xs 0 where
    idc [] n = n
    idc [x] n = n
    idc (x1:(x2:xs)) n = if x2 > x1 then idc (x2:xs) (n+1) else idc (x2:xs) n

main = do
    print(dc [1,2,3,4])
    print(dc [6,2,5,2,1])
    print(dc [1.0,9.5,9.4,10.1,3])
    print(dc [1])
    print(dc ([]::[Int]))
