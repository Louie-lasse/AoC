import Data.Functor ((<&>))
import GHC.SysTools (isContainedIn)
import Prelude hiding (splitAt)

data Game = Game
    { number :: Int
    , rounds :: [Round]
    }
    deriving Show

data Result = Result
    { gameNr :: Int
    , total  :: Round
    }
    deriving Show

data Round = Round
    { red :: Int
    , green :: Int
    , blue :: Int
    }
    deriving Show

main :: IO ()
main = readFile "input.txt" >>= print . sum . map (valueOf . eval . parseGame) . lines

tokenize :: String -> [String]
tokenize = concatMap splitTokens . words where
    splitTokens s | ":" `isContainedIn` s =       [take (length s - 1) s]
                  | ";" `isContainedIn` s =       [take (length s - 1) s]
                  | otherwise             =       [s]

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f as = map reverse $ splitWhen' f as [] where
    splitWhen' f (a:as) cum | f a = cum : splitWhen' f as []
                            | otherwise = splitWhen' f as (a:cum)
    splitWhen' _ _      cum | null cum  = []
                            | otherwise = [cum]

parseGame :: String -> Game
parseGame line | (('G':'a':'m':'e':gNr):rounds) <- splitWhen (\c -> c==';' || c==':') (strip line)
               = Game (read gNr) $ map parseRound rounds

strip :: String -> String
strip = filter (not . (==) ' ')

newRound :: Round
newRound = Round 0 0 0

parseRound :: String -> Round
parseRound s = parseRound' newRound $ map reverse $ splitWhen (==',') s
    where
        parseRound' r (('e':'u':'l':'b'    :num):res) = parseRound' (r{blue  = read $ reverse num}) res
        parseRound' r (('d':'e':'r'        :num):res) = parseRound' (r{red   = read $ reverse num}) res
        parseRound' r (('n':'e':'e':'r':'g':num):res) = parseRound' (r{green = read $ reverse num}) res
        parseRound' r []                              = r
        parseRound' _ s                               = error $ "no match for "++concat s

combine :: Round -> Round -> Round
combine (Round r g b) (Round r' g' b') = Round (max r r') (max g g') (max b b')

maxVal :: [Round] -> Round
maxVal = foldr combine newRound

eval :: Game -> Result
eval (Game n rds) = Result n $ maxVal rds

valueOf :: Result -> Int
valueOf (Result _ r) = red r * green r * blue r