import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Data.Char (isDigit)
import GHC.Data.Maybe (isJust, fromJust)

main :: IO ()
main = readFile "input.txt" >>= print . sum . map readFullInt . lines

readFullInt :: String -> Int
readFullInt s = read $ head ints : [last ints]
              where
                ints = readInt s

readInt :: String -> [Char]
readInt ('o':'n':'e':res) = '1' : readInt ('e' : res)
readInt ('t':'w':'o':res) = '2' : readInt ('o':res)
readInt ('t':'h':'r':'e':'e':res) = '3' : readInt ('e':res)
readInt ('f':'o':'u':'r':res) = '4' : readInt res
readInt ('f':'i':'v':'e':res) = '5' : readInt ('e':res)
readInt ('s':'i':'x':res) = '6' : readInt res
readInt ('s':'e':'v':'e':'n':res) = '7' : readInt ('n':res)
readInt ('e':'i':'g':'h':'t':res) = '8' : readInt ('t':res)
readInt ('n':'i':'n':'e':res) = '9' : readInt ('e':res)
readInt (c:cs) | isDigit c = c  : readInt cs
               | otherwise = readInt cs
readInt _ = []
