import Data.Maybe (mapMaybe)

type Rules = [(Int, [Int])]

type Instr = [Int]

parse :: String -> (Rules, [Instr])
parse s | (rls,_:insts) <- break null $ lines s = (parseRule rls, parseInsts insts) where
    parseInsts = map (read . \l -> '[':l++"]")
    parseRule = foldr (addOne . readRule) []
    readRule :: String -> (Int,Int)
    readRule l | (n1,'|':n2) <- break (=='|') l
               = (read n1, read n2)
    addOne :: (Int,Int) -> Rules -> Rules
    addOne (n1,n2) m | (p1,(_,arr):p2) <- break ((==n1) . fst) m
                     = p1 ++ (n1,n2:arr):p2
                     | otherwise
                     = (n1,[n2]):m
                     
main :: IO()
main = print . sum . map value . filterValid . parse =<< readFile "input.txt"

filterValid :: (Rules, [Instr]) -> [Instr]
filterValid (rls, insts) = filter (isValid rls) insts

isValid :: Rules -> Instr -> Bool
isValid _   []     = True
isValid rls (i:is) | i `notElem` concat (mapMaybe (`lookup` rls) is)
                   = isValid rls is
                   | otherwise = False

value :: Instr -> Int
value i = (!!) i $ (length i - 1) `div` 2