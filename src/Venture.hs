import System.IO

split :: (Char -> Bool) -> [Char] -> [[Char]]
split f s = case dropWhile f s of "" -> []
                                  s' -> w : split f s''
                                    where (w, s'') = break f s'

data Btree a = Lf | Br a (Btree a) (Btree a) deriving Show

insert :: (Num b, Ord a) => a -> Btree (a, b) -> Btree (a, b)
insert x Lf = Br (x,1) Lf Lf
insert x (Br (y,n) l r) 
    | x == y = Br (y,n+1) l r
    | x < y = Br (y,n) (insert x l) r
    | otherwise = Br (y,n) l (insert x r)

insertPair :: (Num b1, Ord b2, Ord a) => (a, b2) -> Btree (a, Btree (b2, b1)) -> Btree (a, Btree (b2, b1))
insertPair (x,y) Lf = Br (x, insert y Lf) Lf Lf
insertPair (x,y) (Br (x',yTree) l r) 
    | x == x' = Br (x', insert y yTree) l r
    | x < x' = Br (x', yTree) (insertPair (x,y) l) r
    | otherwise = Br (x', yTree) l (insertPair (x,y) r)

readLine :: String -> [[Int]]
readLine s = map (map read . split (==',')) (split (\x -> x==' ' || x=='-' || x=='>') s)

getPoints :: [[Int]] -> ((Int, Int), (Int, Int))
getPoints p = ((x,y), (x', y')) where 
    [[x,y],[x',y']] = map (take 2) (take 2 p) 

straight :: (Int,Int) -> (Int,Int) -> Bool 
straight (x,y) (x',y') = (x==x') || (y==y')

direction :: (Int,Int) -> (Int,Int) -> (Int,Int)
direction (x, y) (x', y') = (step x x', step y y') where
    step x y | x == y = 0 | x > y = -1 | otherwise = 1

vecAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecAdd (x, y) (x', y') = (x+x', y+y')

makeLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
makeLine a b = followLine (direction a b) a b where
    followLine d a b 
        | a == b = [a]
        | otherwise = a : followLine d (vecAdd a d) b

count :: Num p => (t -> p) -> Btree (a1, Btree (a2, t)) -> p
count f Lf = 0
count f (Br (x,ytree) l r) = (ycount f ytree) + (count f l) + (count f r) where
    ycount f Lf = 0
    ycount f (Br (y,n) l r) = (f n) + (ycount f l) + (ycount f r)

main = do
    handle <- openFile "data/lines.txt" ReadMode
    contents <- hGetContents handle
    let d = lines contents
    let endlist = map (getPoints . readLine) d
    let pointList = map line endlist 
        line (a, b) = makeLine a b

    let tg = foldl (foldr insertPair) Lf pointList
    print (count (\x -> if x >= 2 then 1 else 0) tg)