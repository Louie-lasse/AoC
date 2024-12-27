import Prelude hiding (Left, Right)
import Control.Applicative ((<|>))
import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust)
data Item = Empty | Visited | Guard | Block
    deriving (Eq)

data Direction = Left | Up | Right | Down
    deriving (Enum, Show)

instance Show Item where
    show Empty = "."
    show Visited = "X"
    show Guard = "^"
    show Block = "#"

type Grid = [[Item]]
type Pos = (Int,Int)
type Movement = Pos -> Pos

parse :: String -> Grid
parse = map (map parse') . lines where
    parse' '.' = Empty
    parse' '^' = Guard
    parse' '#' = Block

main :: IO ()
main = do
    g <- parse <$> readFile "input.txt"
    print $ nVisited $ fullWalk g Up

nVisited :: Grid -> Int
nVisited = sum . map (count (==Visited))

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

onLeft :: (t -> a) -> (t, b) -> (a, b)
onLeft  f (a,b) = (f a,  b)
onRight :: (t -> b) -> (a, t) -> (a, b)
onRight f (a,b) = (  a,f b)

left, right, down, up :: Movement
left  = onRight (flip (-) 1)
right = onRight (+1)
down  = onLeft  (+1)
up    = onLeft  (flip (-) 1)

move :: Direction -> Movement
move = (!!) [left, up, right, down] . fromEnum

turn :: Direction -> Direction
turn = toEnum . (`mod` 4) . (+1) . fromEnum

findGuard :: Grid -> Pos
findGuard g | Just p <- find' 0 g = p 
            | otherwise = error "Guard not found"
          where
    find' n (y:ys) = ((n,) <$> findRow y) <|> find' (n+1) ys
    findRow = elemIndex Guard

(!!!) :: Grid -> Pos -> Maybe Item
g !!! (y,x) |x < 0 || y < 0
            = Nothing
            | (_,r:_) <- splitAt y g
            , (_,e:_) <- splitAt x r
            = Just e
            | otherwise
            = Nothing

place :: Item -> Pos -> Grid-> Grid
place i (y,x) g | (u,r:l) <- splitAt y g
                , (b,_:a) <- splitAt x r
                = u ++ (b ++ i:a) : l

walk :: Grid -> Pos -> Direction -> (Grid, Pos)
walk g p d = (g', last path) where
    path = takeWhile (canMove g) $ iterate (move d) p
    canMove g p = (&&) (isJust (g !!! p)) $ Block /= fromJust (g !!! p)
    g' = foldr (place Visited) g path

fullWalk :: Grid -> Direction -> Grid
fullWalk g d = fullWalk' g (findGuard g) d where
    fullWalk' g p d | (g',p') <- walk g p d
                    , Just Block <- g !!! move d p'
                    = fullWalk' g' p' (turn d)
                    | otherwise
                    = fst $ walk g p d