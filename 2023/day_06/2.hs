{-# LANGUAGE TupleSections #-}
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

type Race = (Integer,Integer)

parse :: [String] -> Race
parse [t,d] = both (read . filter isDigit) (t,d)

main :: IO ()
main = readFile "input.txt" >>= print . alternatives . parse . lines

alternatives :: Race -> Integer
alternatives r | (l,h) <- alt' r = toInteger $ h-l+1 where
    alt' :: Integral a => Race -> (a,a)
    alt' r | (t,d) <- both fromInteger r
           , delta <- sqrt (t*t-4*d) / 2
           , top <- t / 2
           = (ceiling' (top-delta) , floor' (top+delta))

ceiling' :: (RealFrac a1, Integral a2) => a1 -> a2
ceiling' f | rounded f = ceiling f + 1
           | otherwise = ceiling f

floor' :: (RealFrac a1, Integral a2) => a1 -> a2
floor' f   | rounded f = floor f - 1
           | otherwise = floor f

rounded :: RealFrac a => a -> Bool
rounded f = floor f == ceiling f