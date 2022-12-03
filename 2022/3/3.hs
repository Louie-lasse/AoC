import Data.Char
import Data.List

main :: IO ()
main = print . sum . map (charValue . uniqueChar) . lines =<< readFile "text.txt"

charValue :: Char -> Int
charValue c | isLower c = ord c - 96
            | isUpper c = ord c - 38


uniqueChar :: Eq a => [a] -> a
uniqueChar s | (l,r) <- splitAt (length s `div` 2) s = head $ intersect l r