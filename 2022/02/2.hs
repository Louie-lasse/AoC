
data RPC = Rock | Paper | Scissors
    deriving (Show, Enum, Eq)

type Round = (RPC,RPC)

main :: IO Int
main = sum . map (valueRound . parseRound) . lines <$> readFile "text.txt"

valueRound :: Round -> Int
valueRound (o,y) = winner (o,y) + valueRPC y

winner :: Round -> Int
winner (o,y) | y'-o' ==  1 = 6
             | y'-o' == -2 = 6
             | o == y      = 3
             | otherwise   = 0
             where
                (o',y') = both fromEnum (o,y)

valueRPC :: RPC -> Int
valueRPC = (+1) . fromEnum

parseRound :: String -> Round
parseRound s | (o,' ':y) <- break (==' ') s = both parsePlay (o,y)

both :: (t -> b) -> (t, t) -> (b, b)
both f ~(x,y) = (f x, f y)

parsePlay :: String -> RPC
parsePlay s = case s of
    "A" -> Rock
    "X" -> Rock
    "B" -> Paper
    "Y" -> Paper
    "C" -> Scissors
    "Z" -> Scissors