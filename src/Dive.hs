import System.IO

data Command = Forward Int
             | Down Int
             | Up Int

dive :: [Command] -> (Int,Int)
dive coms = follow coms (0,0) where
    follow [] pos = pos
    follow ((Forward n):coms) (x, y) = follow coms (x+n,y)
    follow ((Down n):coms) (x, y) = follow coms (x, y+n)
    follow ((Up n):coms) (x,y) = follow coms (x,y-n)

getSteps :: String -> Int
getSteps (c:[]) = (read [c])::Int
getSteps (c:s) = getSteps s

s2c :: String -> Command
s2c ('f':tl) = Forward (getSteps tl)
s2c ('u':tl) = Up (getSteps tl)
s2c ('d':tl) = Down (getSteps tl)

main = do
    handle <- openFile "commands.txt" ReadMode
    contents <- hGetContents handle
    let commands = map s2c (lines contents)
    let (x,y) = dive commands
    print((x, y, x*y))