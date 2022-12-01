import Data.List

main = do
    text <- lines <$> readFile "text.txt"
    let cals = map sum $ deepMap read $ split text
    let top = take 3 $ reverse $ sort cals
    return $ sum top


deepMap :: (a->b) -> [[a]] -> [[b]]
deepMap f = map (map f)

split :: [String] -> [[String]]
split s | (r,"":r2) <- break (=="") s = r : split r2
        | otherwise = [s]