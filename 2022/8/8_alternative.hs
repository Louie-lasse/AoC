import Data.Char (digitToInt)
import Data.List (transpose)

type Pos = (Int,Int)
type Tree = Int
type Row = [Tree]
type Grid = [Row]

main :: IO ()
main = print . length . visible . parse =<< readFile "text.txt"

parse :: String -> Grid
parse = map (map digitToInt) . lines

positions :: Grid -> [Pos]
positions g = [(x,y) | x <- [0..length g - 1], y <- [0..length (head g) - 1]]

visible :: Grid -> [Pos]
visible g = filter (visibleTree g) $ positions g

visibleTree :: Grid -> Pos -> Bool
visibleTree g (x,y) = left || right || up || down
    where
        right = canSee $ drop y $ g !! x
        left  = canSee $ reverse $ take (y+1) $ g !! x
        down  = canSee $ drop x $ transpose g !! y
        up    = canSee $ reverse $ take (x+1) $ transpose g !! y

canSee :: [Tree] -> Bool
canSee []     = True
canSee (t:ts) = all (< t) ts