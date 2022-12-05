type Pair = (Range,Range)
type Range = (Int,Int)

main = do
    pairs <- map parsePair . lines <$> readFile "text.txt"
    print $ length $ filter overlap pairs 

low  = fst
high = snd

parsePair :: String -> Pair
parsePair = both parseRange . breakAt ','

parseRange :: String -> Range
parseRange = both read . breakAt '-'

overlap ::  Pair -> Bool
overlap (a,b) = overlap' (a,b) || overlap' (b,a)
    where
    overlap' (x,y) = low x `isBetween` y || high x `isBetween` y
    isBetween i (v1,v2) = i >= v1 && i <= v2

breakAt :: Eq a => a -> [a] -> ([a],[a])
breakAt b as | (l,b:r) <- break (==b) as = (l,r)
             | otherwise = (as,[])

both :: (t -> b) -> (t, t) -> (b, b)
both f (a,b) = (f a, f b)