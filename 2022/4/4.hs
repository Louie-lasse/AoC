type Pair = (Range,Range)
type Range = (Int,Int)

main = do
    pairs <- map parsePair . lines <$> readFile "text.txt"
    print $ length $ filter overlap pairs 

low :: (a, b) -> a
low  = fst
high :: (a, b) -> b
high = snd

parsePair :: String -> Pair
parsePair = both parseRange . breakAt ','

parseRange :: String -> Range
parseRange = both read . breakAt '-'

overlap ::  Pair -> Bool
overlap (a,b) | low a <= low b && high a >= high b = True
              | otherwise                          = low a >= low b && high a <= high b

breakAt :: Eq a => a -> [a] -> ([a],[a])
breakAt b as | (l,b:r) <- break (==b) as = (l,r)
             | otherwise = (as,[])

both :: (t -> b) -> (t, t) -> (b, b)
both f (a,b) = (f a, f b)