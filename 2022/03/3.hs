import Data.Char
import Data.List

main :: IO ()
main = print . sum . map (charValue . uniqueChar) . lines =<< readFile "text.txt"

charValue :: Char -> Int
charValue c = ord c - (?) (isLower c) 96 38

(?) :: Bool -> a -> a -> a
True ? a = const a
_    ? _ = id

uniqueChar :: Eq a => [a] -> a
uniqueChar s | (l,r) <- splitAt (length s `div` 2) s = head $ intersect l r