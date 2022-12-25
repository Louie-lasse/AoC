import Data.List (nub)
type State = [Pos]
type Pos = (Int,Int)
data Instr = U | D | L | R
    deriving (Show, Eq)

main :: IO ()
main = print
     . length . nub . map (!!9)
     . applyAll . concatMap parse
     . reverse . lines =<< readFile "text.txt"

initialState :: State
initialState = replicate 10 (0,0)

applyAll :: [Instr] -> [State]
applyAll []     = [initialState]
applyAll (i:is) = apply i h : h : hs
    where
        (h:hs) = applyAll is

apply :: Instr -> State -> State
apply dir (h:ts) = trail $ h `move` dir : ts

trail :: [Pos] -> [Pos]
trail (h:n:ts) | h `farFrom` n = h : trail (n `moveTo` h : ts)
               | otherwise     = h : trail (n            : ts)
trail ts                       = ts

farFrom :: Pos -> Pos -> Bool
farFrom (x1,y1) (x2,y2) = abs (x1-x2) >= 2 || abs (y1-y2) >= 2

move :: Pos -> Instr -> Pos
move (x,y) U = (x+1,y)
move (x,y) D = (x-1,y)
move (x,y) L = (x,y-1)
move (x,y) R = (x,y+1)

moveTo :: Pos -> Pos -> Pos
(x1,y1) `moveTo` (x2,y2) = (x1 `closerTo` x2, y1 `closerTo` y2)
    where
        a `closerTo` b | a > b = a - 1
                       | otherwise = a + fromEnum (a < b)
        

parse :: String -> [Instr]
parse s | (dir,' ':is) <- break (==' ') s = replicate (read is) $ parse' dir
    where
        parse' "U" = U
        parse' "D" = D
        parse' "L" = L
        parse' "R" = R