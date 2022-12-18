import Data.List (transpose)
import Data.Char (digitToInt)

type Pos = (Int,Int)
type Tree = Int
type Row = [Tree]
type Grid = [Row]

main :: IO ()
main = do
    g <- parse <$> readFile "text.txt"
    let vals = flist ([visibleFrom (x,y) | x <- [0..length g-1], y <- [0..length (head g)-1]]) g 
    print $ maximum vals

parse :: String -> Grid
parse = map (map digitToInt) . lines

visibleFrom :: Pos -> Grid -> Int
visibleFrom (x,y) g = left * right * up * down
    where
        right = view $ drop y $ g !! x
        left  = view $ reverse $ take (y+1) $ g !! x
        down  = view $ drop x $ transpose g !! y
        up    = view $ reverse $ take (x+1) $ transpose g !! y

view :: [Tree] -> Int
view []     = 0
view (t:ts) = fromEnum (any (>=t) ts) + length (takeWhile (<t) ts)

flist :: [p -> b] -> p -> [b]
flist fs a = map ($ a) fs