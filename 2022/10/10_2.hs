type Register = Int
data Instruction = NOP | Add Int
    deriving (Eq,Show)

x :: Register
x = 1

rowLength :: Integer
rowLength = 40

main = do
    instrs <- ordered . map parse . lines <$> readFile "text.txt"
    mapM_ print $ deCode x $ chunksOf 40 instrs

startRow :: String
startRow = replicate 40 ' '

deCode :: Register -> [[Instruction]] -> [String]
deCode _ []                                        = []
deCode x (r:rs) | (x',s) <- showRow 0 startRow r x = s : deCode x' rs

showRow :: Register -> String -> [Instruction] -> Register -> (Register,String)
showRow _  row []     x                        = (x,row)
showRow pc row (i:is) x | abs (newX - pc) <= 1 = showRow (pc+1) (light pc row) is newX
                        | otherwise = showRow (pc+1) row is newX
    where
        newX = x `apply` i
        
light :: Register -> String -> String
light i = setAt i '#'

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

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i as = take i as : chunksOf i (drop i as)

setAt :: Int -> a -> [a] -> [a]
setAt i a ls | i < 0 = ls
             | otherwise = go i ls
  where
    go 0 (_:xs) = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []