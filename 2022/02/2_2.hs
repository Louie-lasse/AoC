
data RPC = Rock | Paper | Scissors
    deriving (Show, Enum, Eq)

data Result = Lose | Draw | Win
    deriving Enum

type Round = (RPC,Result)

main = sum . map (valueRound . parseRound) . lines <$> readFile "text.txt"

valueRound :: Round -> Int
valueRound (o,y) = valueResult y + valueRPC (o,y)

valueResult :: Result -> Int
valueResult = (*3) . fromEnum

valueRPC :: Round -> Int
valueRPC (o,Win)  = 1 + ((fromEnum o + 1) `mod` 3)
valueRPC (o,Lose) = 1 + ((fromEnum o - 1) `mod` 3)
valueRPC (o,Draw) = 1 + fromEnum o

parseRound :: String -> Round
parseRound s | (o,' ':y) <- break (==' ') s = (parsePlay o, parseResult y)

both :: (t -> b) -> (t, t) -> (b, b)
both f ~(x,y) = (f x, f y)

parsePlay :: String -> RPC
parsePlay s = case s of
    "A" -> Rock
    "B" -> Paper
    "C" -> Scissors

parseResult :: String -> Result
parseResult s = case s of
    "Z" -> Win
    "Y" -> Draw
    "X" -> Lose