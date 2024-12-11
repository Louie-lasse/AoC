import Data.Maybe (catMaybes)
import Data.List (sort)

data L = X | M | A | S
    deriving (Read, Eq, Ord, Show)

type Grid = [[L]]
type Coord = (Int,Int)

parse :: [String] -> Grid
parse = map $ map $ read . (:[])

main = print . count . parse . lines =<< readFile "input.txt"

count :: Grid -> Int
count g = length $ filter id $ map (search g) (coords g)

neighbors :: Coord -> [Coord]
neighbors (x,y) = 
    [ (x-1,y-1)
    , (x+1,y+1)
    , (x+1,y-1)
    , (x-1,y+1)
    ]

coords :: Foldable t => [t a] -> [Coord]
coords g = concat [map (n,) [0..length (head g)-1] | n <- [0..length g-1]]

search :: Grid -> Coord -> Bool
search g c | Just A <- g !!! c
           = (==) ([M,S],[M,S]) $ both (sort . catMaybes) $ splitAt 2 $ map (g !!!) (neighbors c) 
search _ _ = False

both :: (t -> b) -> (t, t) -> (b, b)
both f (a,b) = (f a, f b)

(!!!) :: [[a]] -> Coord -> Maybe a
g !!! (x,y) | x < 0 || y < 0 = Nothing
            | (_,r:_) <- splitAt x g
            , (_,e:_) <- splitAt y r
            = Just e
            | otherwise = Nothing