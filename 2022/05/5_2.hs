type Stack = [Char]
type StackNr = Int
type Instruction = (Int,StackNr,StackNr)

main :: IO ()
main = do
    (upDock,upInstrs) <- breakAt [] . lines <$> readFile "text.txt"
    let dock = parseDock $ init upDock
    let instructions = reverse $ map parseInstruction upInstrs
    print $ map head $ foldr doInstruction dock instructions

amount :: Instruction -> Int
amount (a,_,_) = a
from :: Instruction -> Int
from (_,f,_) = f
to :: Instruction -> Int
to (_,_,t) = t

doInstruction :: Instruction -> [Stack] -> [Stack]
doInstruction (a,f,t) s | f <= t = top ++ mid ++ (take a flist ++ tList) : end
    where
        (top,flist:tmp) = splitAt f s
        (mid,tList:end) = splitAt (t-f) (drop a flist:tmp)
doInstruction (a,f,t) s = top ++ (take a fList ++ tList) : mid ++ drop a fList : end
    where
        (top,tList:tmp) = splitAt t s
        (mid,fList:end) = splitAt (f-t-1) tmp

parseInstruction :: String -> Instruction
parseInstruction s = (read amount, read from -1, read to -1)
    where
        (_:amount:_:from:_:[to]) = splitList ' ' s

parseDock :: [String] -> [Stack]
parseDock s = foldr parseDock' (replicate nDocks []) s
    where 
        nDocks       = (length (head s) + 1) `div` 4
        parseDock'  ""                []     = []
        parseDock' ('[':c:']':res)   (s:ss) = (c:s) : parseDock' (rest res) ss
        parseDock' (' ':' ':' ':res) (s:ss) = s : parseDock' (rest res) ss
        rest (' ':r) = r
        rest r       = r

breakAt :: Eq a => a -> [a] -> ([a],[a])
breakAt b as | (l,b:r) <- break (==b) as = (l,r)
             | otherwise = (as,[])

splitList :: Eq a => a -> [a] -> [[a]]
splitList _   [] = []
splitList sep list = h:splitList sep t
        where (h,t)=breakAt sep list