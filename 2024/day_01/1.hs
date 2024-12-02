
main = print . solve . map parse . lines =<< readFile "input.txt"

solve :: (Num a, Ord a) => [(a, a)] -> a
solve = sum . uncurry (zipWith diff) . both qs . unzip

diff :: (Ord a, Num a) => a -> a -> a
diff a b | a < b = b - a
         | otherwise = a - b

parse :: String -> (Int, Int)
parse s | [a,b] <- words s = both read (a, b)

qs :: Ord a => [a] -> [a]
qs [] =[]
qs (a:as) = qs [v | v<-as, v < a] ++ a : qs [v | v<-as, v >=a]

both :: (t -> b) -> (t, t) -> (b, b)
both f (a,b) = (f a, f b)