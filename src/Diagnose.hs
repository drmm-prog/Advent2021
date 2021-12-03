import System.IO

s2il :: [Char] -> [Int]
s2il str = let rStr = reverse str in
    map char2Int rStr where
        char2Int '0' = 0
        char2Int '1' = 1
        char2Int _ = 0

b2i :: [Int] -> Int
b2i = accB2I 0 1 where
    accB2I out pow [] = out
    accB2I out pow (x:xs) = accB2I (out + x*pow) (pow*2) xs

ladd :: [Int] -> [Int] -> [Int]
ladd l [] = l 
ladd [] l = l
ladd (x:xs) (y:ys) = (x+y) : ladd xs ys

geCount :: [String] -> ([Int], Int)
geCount = accCount ([], 0) where
    accCount (sum, len) [] = (sum, len)
    accCount (sum, len) (bin:tl) = accCount (ladd sum (s2il bin), len+1) tl

invBin :: [Int] -> [Int]
invBin = map (\x -> 1 - x)

getGamma :: Num a => [String] -> [a]
getGamma binaries = 
    let (sum, len) = geCount binaries in
        sum2gam sum len where
            sum2gam [] _ = []
            sum2gam (x:xs) len = if x > (len - x) then 1 : sum2gam xs len else 0 : sum2gam xs len

main = do
    handle <- openFile "data/bins.txt" ReadMode
    contents <- hGetContents handle
    let g = getGamma (lines contents)
    print(b2i g * b2i (invBin g))