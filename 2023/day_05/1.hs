import Data.Functor ((<&>))
import Data.Char (isDigit)
import qualified Data.List as L
import Data.Bifunctor (second, first)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Map as M hiding ((!))
import Control.Applicative ((<|>))

both :: (t -> b) -> (t, t) -> (b, b)
both f (a,b) = (f a, f b)

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f as = map reverse $ splitWhen' f as [] where
    splitWhen' f (a:as) cum | f a = cum : splitWhen' f as []
                            | otherwise = splitWhen' f as (a:cum)
    splitWhen' _ _      cum | null cum  = []
                            | otherwise = [cum]

type SeedLookup = [SeedMap]
data SeedMap    = Map 
    { from  :: Int
    , to    :: Int
    , range :: Int
    }
    deriving Show

data Almanac = Almanac
    { seeds :: [Int]
    , soil :: SeedLookup
    , fertilizer :: SeedLookup
    , water :: SeedLookup
    , light :: SeedLookup
    , temp :: SeedLookup
    , humidity :: SeedLookup
    , location :: SeedLookup
    }
    deriving (Show)

parse :: [String] -> Almanac
parse s = a'''''''
    where
        (seeds:maps) = splitWhen null s
        seeds' | (_,' ':res) <- break (==' ') (head seeds) = parseNums res
        maps' = map (parseMap . tail) maps
        a  = Almanac seeds'
        a' = a $ head maps'
        a'' = a' $ maps' !! 1
        a''' = a'' $ maps' !! 2
        a'''' = a''' $ maps' !! 3
        a''''' = a'''' $ maps' !! 4
        a'''''' = a''''' $ maps' !! 5
        a''''''' = a'''''' $ maps' !! 6

parseMap :: [String] -> SeedLookup
parseMap = map (toMap . parseNums) where
    toMap [a,b,c] = Map b a c

addToMap :: [Int] -> M.Map Int Int -> M.Map Int Int
addToMap (d:s:[r]) m = foldr (uncurry M.insert) m $zip [s..s+r-1] [d..d+r-1]
addToMap res m = error $ show res

parseNums :: String -> [Int]
parseNums = map read . splitWhen (==' ')

main :: IO ()
main = readFile "input.txt" >>= print . lowest . parse . lines

lowest :: Almanac -> Int
lowest a = minimum $ map (seedLocation a) $ seeds a

get :: (Almanac -> SeedLookup) -> Almanac -> Int -> Int
get f a i | (a:as) <- filter ((<=i) . from) (f a)
          , best <- head $ L.sortOn ((0-) . from) (a:as) 
          , from best + range best >= i
          = to best + i - from best
          | otherwise = i

seedLocation :: Almanac -> Int -> Int
seedLocation a s = get location   a $
                   get humidity   a $
                   get temp       a $
                   get light      a $
                   get water      a $
                   get fertilizer a $
                   get soil       a s