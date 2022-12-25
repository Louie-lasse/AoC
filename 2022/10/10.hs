type Register = Int
data Instruction = NOP | Add Int
    deriving (Eq,Show)

x :: Register
x = 1

breakPoints :: [Int]
breakPoints = [20,60,100,140,180,220]

main :: IO ()
main = do
    instrs <- ordered . map parse . lines <$> readFile "test.txt"
    let checks = map (($ instrs) . take) breakPoints
    print $ sum $ zipWith (*) breakPoints $  map calculate checks

calculate :: [Instruction] -> Register
calculate = foldl apply x

apply :: Register -> Instruction -> Register
apply x i = x + value i

value :: Instruction -> Int
value (Add n) = n
value _       = 0

ordered :: [Instruction] -> [Instruction]
ordered = (:) NOP . ordered'
    where
        ordered' (NOP:r) = NOP: ordered' r
        ordered' (add:r) = NOP:add: ordered' r

parse :: String -> Instruction
parse ('n':_) = NOP
parse s       | ("addx",' ':n) <- break (==' ') s = Add $ read n
              | otherwise                         = error $ "Failed to parse "++s