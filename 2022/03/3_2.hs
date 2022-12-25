import Data.Char
import Data.List

main :: IO ()
main = print . sum . map groupValue . groupOf 3 . lines =<< readFile "text.txt"

charValue :: Char -> Int
charValue c | isLower c = ord c - 96
            | isUpper c = ord c - 38

groupValue :: [String] -> Int
groupValue (s:ss) = charValue $ head $ foldr intersect s ss

groupOf :: Int -> [a] -> [[a]]
groupOf _ [] = []
groupOf i ls | (l,r) <- splitAt i ls = l : groupOf i r