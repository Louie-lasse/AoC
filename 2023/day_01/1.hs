import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Data.Char (isDigit)

main :: IO ()
main = readFile "input.txt" >>= print . sum . map readFullInt . lines

readFullInt :: String -> Int
readFullInt = read . ends . filter isDigit
    where
        ends as = head as : [last as]