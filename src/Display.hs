import System.IO

data DisplayDigit = Digit String Int deriving Show

get1478 :: String -> [DisplayDigit] -> [DisplayDigit]
get1478 s ds
    | length s == 7 = Digit s 8 : ds
    | length s == 2 = Digit s 1 : ds
    | length s == 3 = Digit s 7 : ds
    | length s == 4 = Digit s 4 : ds
    | otherwise = ds

getComplement :: String -> String
getComplement s = complement "abcdefg" s where 
    complement set [] = set
    complement [] s = []
    complement (x:xs) s
        | member s x = complement xs s
        | otherwise = x : complement xs s

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset [] ys = True
isSubset (x:xs) ys
    | not (member ys x) = False
    | otherwise = isSubset xs ys

getByNum :: Int -> [DisplayDigit] -> String
getByNum n ((Digit s x):ds) = if n == x then s else getByNum n ds
getByNum n [] = ""

getByString :: [DisplayDigit] -> String -> Int
getByString ((Digit s' n):ds) s = if (length s == length s') && isSubset s s' then n else getByString ds s
getByString [] s = -1

get069 :: String -> [DisplayDigit] -> [DisplayDigit]
get069 s ds = 
    let 
        c = head (getComplement s)
        seven = getByNum 7 ds
        four = getByNum 4 ds
    in
        if member seven c then Digit s 6 : ds
            else if member four c then Digit s 0 : ds
                else Digit s 9 : ds

get235 :: String -> [DisplayDigit] -> [DisplayDigit]
get235 s ds = 
    let
        one = getByNum 1 ds
        six = getByNum 6 ds
    in 
        if isSubset s six then Digit s 5 : ds
            else if isSubset one s then Digit s 3 : ds
                else Digit s 2 : ds
        
split :: (Char -> Bool) -> [Char] -> [[Char]]
split f s = case dropWhile f s of "" -> []
                                  s' -> w : split f s''
                                    where (w, s'') = break f s'

parseInput :: String -> ([String],[String])
parseInput line =
    let 
        [x,y] = take 2 (split (=='|') line)
    in
        (split (==' ') x, split (==' ') y)

member :: Eq t => [t] -> t -> Bool
member [] x = False 
member (y:ys) x
    | x == y = True
    | otherwise = member ys x

count1478 :: [String] -> Int
count1478 displays = sum (map ((\x -> if member [7,2,3,4] x then 1 else 0) . length) displays)

getRight :: (a, a) -> a
getRight (x, y) = y

getLeft :: (a, a) -> a
getLeft (x, y) = x

getNums :: [String] -> [DisplayDigit]
getNums inputs =
    let 
        easy = foldr get1478 [] inputs
        medium = foldr get069 easy (filter (\x -> length x == 6) inputs)
        hard = foldr get235 medium (filter (\x -> length x == 5) inputs)
    in
        hard

valueDisplay :: [String] -> [DisplayDigit] -> Int
valueDisplay display digits = 
    let 
        nums = map (getByString digits) display
    in
        createInt nums 0 where
            createInt [] n = n
            createInt (x:xs) n = createInt xs (n*10 + x)

translateLine :: ([String],[String]) -> Int
translateLine (input, output) = 
    let 
        digits = getNums input
    in
        valueDisplay output digits

main = do
    handle <- openFile "data/display.txt" ReadMode
    contents <- hGetContents handle
    let inputLines = map parseInput (lines contents)
    let output = map getRight inputLines
    print (sum (map count1478 output))
    print (sum(map translateLine inputLines))