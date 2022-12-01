main :: IO Integer
main = do
    nums <- map read . lines <$> readFile "test.txt"
    return $ sum . map fuelReq $ nums
    
fuelReq :: Integral t => t -> t
fuelReq n   | fuel <= 0 = 0
            | otherwise = fuel + fuelReq fuel
            where
                fuel = n `div` 3 - 2