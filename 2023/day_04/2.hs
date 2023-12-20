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

type Card = (Int, [Int], [Int])

parse :: String -> Card
parse s | (_, ':':' ':ln) <- break (== ':') s
        , (win, '|':' ':my) <- break (=='|') ln
        , (pWin,pMy) <- both (map read . filter (not . null) . splitWhen (==' ')) (win,my)
        = (1, pWin, pMy)


main :: IO ()
main = readFile "input.txt" >>= print . score . map parse . lines

score :: [Card] -> Int
score = sum . map amount . score'

score' :: [Card] -> [Card]
score' [] = []
score' (c:cds) | cds' <- copy (winAmount c) (amount c) cds
               = c : score' cds'
    where
        winAmount (_,win,my) = length $ win `L.intersect` my

amount :: Card -> Int
amount (a,_,_) = a

copy :: Int -> Int -> [Card] -> [Card]
copy n amnt cds = map inc (take n cds) ++ drop n cds
    where inc (a,w,m) = (a+amnt,w,m)