import System.IO

split f s = case dropWhile f s of "" -> []
                                  s' -> w : split f s''
                                    where (w, s'') = break f s'



-- load a bingo board
readBoard :: [String] -> [[[(Int, Bool)]]]
readBoard [] = []
readBoard ("" : tl) = readBoard tl
readBoard (row : tl) = readRows 5 (row:tl) : readBoard (drop 5 (row:tl)) where 
    readRows 0 rows = []
    readRows n (row:tl) = (map (\x -> (read x, False)) (split (==' ') row) :: [(Int,Bool)]) : readRows (n-1) tl
    readRows n [] = []

checkRow [] = True
checkRow ((_,False):xs) = False
checkRow ((_,True):xs) = checkRow xs

getRight (x,y) = y
getLeft (x,y) = x

checkColumns [] = False 
checkColumns ([]:xs) = False 
checkColumns rows = (foldr (&&) True (map (getRight . head) rows)) || checkColumns (map tail rows)

checkBoard :: [[(a,Bool)]] -> Bool  
checkBoard board = checkColumns board || or (map checkRow board)    

updateBoard :: Eq a => a -> [[(a,Bool)]] -> [[(a,Bool)]]
updateBoard val = map (map (updateSq val)) where 
    updateSq val (x,b) = if  x==val then (x,True) else (x,b) 

valueBoard :: Num a => [[(a,Bool)]] -> a
valueBoard = foldr bSum 0 where 
    bSum [] n = n
    bSum ((a,False):xs) n = bSum xs (a+n)
    bSum ((a,True):xs) n = bSum xs n 


mask bools vals = getRight (unzip (filter getLeft (zip bools vals)))

playSequence :: [Int] -> [[[(Int,Bool)]]] -> Maybe Int
playSequence [] boards = Nothing 
playSequence (n:ns) boards = 
    let
        newBoards = map (updateBoard n) boards
        success = map checkBoard newBoards
    in 
        if or success then Just (n * valueBoard (head (mask success newBoards))) else playSequence ns newBoards
            
getLastBoard :: [Int] -> [[[(Int,Bool)]]] -> Maybe Int
getLastBoard = accGetLast 0 [] where
    accGetLast n b [] _ = Just (n * valueBoard b)
    accGetLast n b ns [] = Just (n * valueBoard b)
    accGetLast x b (n:ns) boards = 
        let
            newBoards = map (updateBoard n) boards
            success = map checkBoard newBoards
            b' = if winners == [] then b else head winners where
                winners = mask success newBoards
        in 
            if or success then accGetLast n b' ns (mask (map not success) newBoards) else accGetLast n b' ns newBoards
 
                    

main = do
    handle <- openFile "data/bingo.txt" ReadMode
    contents <- hGetContents handle
    let d = lines contents
    let nums = map read (split (==',') (head d)) ::[Int]
    let boards = readBoard (tail d) 
    print (playSequence nums boards)
    print (getLastBoard nums boards)