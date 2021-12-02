import System.IO

data Command = Forward Int
             | Down Int
             | Up Int

dive :: [Command] -> (Int,Int,Int)
dive coms = follow coms (0,0,0) where
    follow [] pos = pos
    follow ((Forward n):coms) (a, x, y) = follow coms (a, x+n, y + a*n)
    follow ((Down n):coms) (a, x, y) = follow coms (a+n, x, y)
    follow ((Up n):coms) (a, x, y) = follow coms (a-n, x, y)

getSteps :: String -> Int
getSteps (c:[]) = (read [c])::Int
getSteps (c:s) = getSteps s

s2c :: String -> Command
s2c ('f':tl) = Forward (getSteps tl)
s2c ('u':tl) = Up (getSteps tl)
s2c ('d':tl) = Down (getSteps tl)

main = do
    handle <- openFile "data/commands.txt" ReadMode
    contents <- hGetContents handle
    let commands = map s2c (lines contents)
    let (a,x,y) = dive commands
    print((a, x, y, x*y))