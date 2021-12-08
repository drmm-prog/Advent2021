import System.IO

split :: (Char -> Bool) -> [Char] -> [[Char]]
split f s = case dropWhile f s of "" -> []
                                  s' -> w : split f s''
                                    where (w, s'') = break f s'

fuel :: Int -> [Int] -> Int
fuel n pos = sum (map (abs . (\x -> x - n)) pos)

fuel2 :: Int -> [Int] -> Int
fuel2 n pos = sum (map cost pos) where
    cost p = div (x*(x+1)) 2 where
        x = abs (p - n)

minFuel :: Int -> [Int] -> (Int -> [Int] -> Int) -> Int
minFuel 0 pos f = f 0 pos
minFuel n pos f = min (f n pos) (minFuel (n-1) pos f)

main :: IO ()
main = do
    handle <- openFile "data/crabs.txt" ReadMode
    contents <- hGetContents handle
    let crabs = map read (split (==',') contents) :: [Int]
    let max = maximum crabs
    print (minFuel max crabs fuel)
    print (minFuel max crabs fuel2)