import System.IO

split :: (Char -> Bool) -> [Char] -> [[Char]]
split f s = case dropWhile f s of "" -> []
                                  s' -> w : split f s''
                                    where (w, s'') = break f s'

insertFish :: Int -> Int -> [Int] -> [Int]
insertFish n _ [] = []
insertFish n 0 (x:xs) = (x+n):xs
insertFish n i (x:xs) = x : insertFish n (i-1) xs

genFishArray :: Int -> [Int]
genFishArray 0 = []
genFishArray n = 0 : genFishArray (n-1)

updateFishPop :: [Int] -> [Int]
updateFishPop fish =
    let
        n0 = head fish
    in 
        insertFish n0 6 (tail fish ++ [n0])

napply :: (Eq t1, Num t1) => t1 -> (t2 -> t2) -> t2 -> t2
napply 0 f d = d
napply n f d = napply (n-1) f (f d)

main = do
    handle <- openFile "data/fish.txt" ReadMode
    contents <- hGetContents handle
    let fish = map read (split (==',') contents) :: [Int]
    print fish
    let fishPop = foldr (insertFish 1) (genFishArray 9) fish
    print fishPop
    print (sum (napply 256 updateFishPop fishPop))