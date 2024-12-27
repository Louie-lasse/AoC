import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)

type Equ = (Int,[Int])

parse :: String -> [Equ]
parse = map parse' . lines where
    parse' s | (r,':':' ':ns) <- break (==':') s
             = (read r, map read $ words ns)

main :: IO ()
main = print . sum . mapMaybe calculate . parse =<< readFile "input.txt"

calculate :: Equ -> Maybe Int
calculate (r,i:ins) = calc r i ins where
    calc r a [] | r == a = Just a
                | otherwise = Nothing
    calc r a (i:is) = calc r (a+i) is <|> calc r (a*i) is