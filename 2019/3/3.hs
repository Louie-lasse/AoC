import Data.List (sort)
data Instr = U Int | D Int | L Int | R Int
    deriving Eq

main = do
    unParsed <- lines <$> readFile "test.txt"
    let [red,blue] = map parse unParsed
    let rWire = walkFromStart red
    let bWire = walkFromStart blue
    let intersections = intersect rWire $ tail bWire
    return $ minimum $ map (uncurry (+) . both abs) intersections

parse :: String -> [Instr]
parse = map parse' . split
    where
        parse' :: String -> Instr
        parse' ('U':res) = U $ read res
        parse' ('D':res) = D $ read res
        parse' ('R':res) = R $ read res
        parse' ('L':res) = L $ read res

both :: (t -> b) -> (t, t) -> (b, b)
both f (a,b) = (f a, f b)

split :: String -> [String]
split s | (r,',':r2) <- break (==',') s = r : split r2
            | otherwise = [s]

walkFromStart :: [Instr] -> [(Int, Int)]
walkFromStart = walk (0,0)

instr1 = [L 5, D 3]

walk :: (Int, Int) -> [Instr] -> [(Int, Int)]
walk (_,_) []          = []
walk (a,b) ((U n):res) = [(a',b) | a' <- [a..a+n]] ++ walk (a+n,b) res
walk (a,b) ((D n):res) = [(a',b) | a' <- [a-n..a]] ++ walk (a-n,b) res
walk (a,b) ((R n):res) = [(a,b') | b' <- [b..b+n]] ++ walk (a,b+n) res
walk (a,b) ((L n):res) = [(a,b') | b' <- [b-n..b]] ++ walk (a,b-n) res


intersect               :: (Eq a) => [a] -> [a] -> [a]
intersect               =  intersectBy (==)

intersectBy             :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy _  [] _     =  []
intersectBy _  _  []    =  []
intersectBy eq xs ys    =  [x | x <- xs, any (eq x) ys]