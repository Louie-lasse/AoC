import Data.List (nub)

main :: IO ()
main = print . value =<< readFile "text.txt"

value :: String -> Int
value s | length ff == length (nub ff) = 14
        | otherwise                    = 1 + value (tail s)
        where
            ff = take 14 s