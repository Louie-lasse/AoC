import Data.Functor ((<&>))
import Data.Char (isDigit)
import qualified Data.List as L
import Data.Bifunctor (second, first)
import Data.Maybe (mapMaybe)

both :: (t -> b) -> (t, t) -> (b, b)
both f (a,b) = (f a, f b)

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f as = map reverse $ splitWhen' f as [] where
    splitWhen' f (a:as) cum | f a = cum : splitWhen' f as []
                            | otherwise = splitWhen' f as (a:cum)
    splitWhen' _ _      cum | null cum  = []
                            | otherwise = [cum]

type Card = ([Int], [Int])

parse :: String -> Card
parse s | (_, ':':' ':ln) <- break (== ':') s
        , (win, '|':' ':my) <- break (=='|') ln
        , (pWin,pMy) <- both (map read . filter (not . null) . splitWhen (==' ')) (win,my)
        = (pWin, pMy)

main :: IO ()
main = readFile "input.txt" >>= print . sum . map (score . parse) . lines

score :: Card -> Int
score (win,my) | (_:rs) <- win `L.intersect` my
                 = product $ replicate (length rs) 2
                 | otherwise = 0