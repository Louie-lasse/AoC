
main = print . solve . map parse . lines =<< readFile "input.txt"

solve is | (a,b) <- unzip is
         = sum $ map (value b) a

value :: [Int] -> Int -> Int
value bs a = a * length (filter (a==) bs)

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