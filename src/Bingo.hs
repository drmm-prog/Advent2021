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

playSequence :: [Int] -> [[[(Int,Bool)]]] -> Maybe Int
playSequence [] boards = Nothing 
playSequence (n:ns) boards = 
    let
        newBoards = map (updateBoard n) boards
        success = map checkBoard newBoards
    in 
        if or success then getBoardResult success newBoards else playSequence ns newBoards where
            getBoardResult [] bs = Nothing
            getBoardResult (False:xs) bs = getBoardResult xs (tail bs)
            getBoardResult (True:xs) bs = Just (n * valueBoard (head bs))
            

main = do
    handle <- openFile "data/bingo.txt" ReadMode
    contents <- hGetContents handle
    let d = lines contents
    let nums = map read (split (==',') (head d)) ::[Int]
    let boards = readBoard (tail d) 
    print (playSequence nums boards)