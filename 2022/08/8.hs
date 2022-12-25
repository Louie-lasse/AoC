import Data.Char (digitToInt)
import Data.List (transpose, nub)
import Data.Tuple (swap)

type Pos = (Int,Int)
type Tree = Int
type Row = [Tree]
type Grid = [Row]
type Direction = Pos -> Pos

main :: IO ()
main = print . length . nub . visible . parse =<< readFile "text.txt"

parse :: String -> Grid
parse = map (map digitToInt) . lines

deeper, shallower :: Direction
deeper    (x,y) = (x,y+1)
shallower (x,y) = (x,y-1)

visible :: Grid -> [Pos]
visible g = concat [fromLeft, fromRight, fromTop, fromBottom]
    where
        fromLeft  = visibleInGrid (0,0) deeper g
        fromRight = visibleInGrid (0,maxY) shallower $ map reverse g
        fromTop = map swap $ visibleInGrid (0,0) deeper $ transpose g
        fromBottom = map swap $ visibleInGrid (0,maxX) shallower $ map reverse $ transpose g
        maxX = length g - 1
        maxY = length (head g) - 1

visibleInGrid :: Pos -> Direction -> Grid -> [Pos]
visibleInGrid _     d []     = []
visibleInGrid (x,y) d (r:rs) = visibleInRow (-1) (x,y) d r ++ visibleInGrid (x+1,y) d rs

visibleInRow :: Int -> Pos -> Direction -> Row -> [Pos]
visibleInRow _ _ _ []                 =     []
visibleInRow h p d (i:is) | i > h     = p : visibleInRow i (d p) d is
                          | otherwise =     visibleInRow h (d p) d is


