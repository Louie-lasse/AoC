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
type SeedRule = (Int,Int)
data SeedMap    = Map
    { from  :: Int
    , to    :: Int
    , range :: Int
    }
    deriving Show

data Almanac = Almanac
    { seeds :: [SeedRule]
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
        seeds' | (_,' ':res) <- break (==' ') (head seeds) = parseSeeds $ parseNums res
        maps' = map (parseMap . tail) maps
        a  = Almanac seeds'
        a' = a $ head maps'
        a'' = a' $ maps' !! 1
        a''' = a'' $ maps' !! 2
        a'''' = a''' $ maps' !! 3
        a''''' = a'''' $ maps' !! 4
        a'''''' = a''''' $ maps' !! 5
        a''''''' = a'''''' $ maps' !! 6

parseSeeds :: Num b => [b] -> [(b, b)]
parseSeeds (a:b:res) = (a,a+b-1) : parseSeeds res
parseSeeds []        = []

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
lowest a = minimum $ map fst $ seedLocation a (seeds a)

get :: (Almanac -> SeedLookup) -> Almanac -> SeedRule -> [SeedRule]
get f a sr | a <- filter ((<= snd sr) . from) (f a)
           , b <- filter ((>= fst sr) . (\s -> from s + range s)) a
           = splitRule sr $ L.sortOn from b

splitRule :: SeedRule -> [SeedMap] -> [SeedRule]
splitRule (l,h) (Map f t r:res)
    | l < f
    = (l,f-1) : splitRule (f,h) (Map f t r:res)
    | h > f+r-1
    = both ((t-f) +) (l,f+r-1) : splitRule (f+r,h) res
    | otherwise
    = [both ((t-f) +) (l,h)]
splitRule (l,h) _ = [(l,h)]

seedLocation :: Almanac -> [SeedRule] -> [SeedRule]
seedLocation a s = concatMap (get location   a) $
                   concatMap (get humidity   a) $
                   concatMap (get temp       a) $
                   concatMap (get light      a) $
                   concatMap (get water      a) $
                   concatMap (get fertilizer a) $
                   concatMap (get soil       a) s