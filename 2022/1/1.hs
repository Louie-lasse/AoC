import Data.List

main = do
    text <- lines <$> readFile "text.txt"
    let cals = map (sum .map read) $ split text
    return $ sum $ take 3 $ reverse $ sort cals

split :: [String] -> [[String]]
split s | (r,"":r2) <- break (=="") s = r : split r2
        | otherwise = [s]