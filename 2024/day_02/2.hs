
main = print . length .  filter safe . map (map read . words) . lines =<< readFile "input.txt"

safe :: [Int] -> Bool
safe r = inc r || dec r

inc :: [Int] -> Bool
inc = safeRep (\a b -> a < b && b - a <= 3)

dec :: [Int] -> Bool
dec = safeRep (\b a -> a < b && b - a <= 3)

safeRep f (a:a':s) | f a a' = safe' f (a:a':s)
                   | otherwise = safeCon f (a:s) || safeCon f (a':s)
safeRep _ _ = True

safe' f (a:a':a'':s) | f a' a'' = safe' f (a':a'':s) 
                     | otherwise = safeCon f (a:a'':s) || safeCon f (a':s)
safe' _ _ = True

safeCon f l = and $ zipWith f l $ tail l