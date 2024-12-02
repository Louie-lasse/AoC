
main = print . length .  filter safe . map (map read . words) . lines =<< readFile "input.txt"

safe :: [Int] -> Bool
safe r = inc r || dec r

inc :: [Int] -> Bool
inc = safeRep (\a b -> a < b && b - a <= 3)

dec :: [Int] -> Bool
dec = safeRep (\b a -> a < b && b - a <= 3)

safeRep :: (b -> b -> Bool) -> [b] -> Bool
safeRep cond r = and $ zipWith cond r $ tail r