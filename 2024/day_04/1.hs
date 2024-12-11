import Data.Maybe (catMaybes)

data L = X | M | A | S
    deriving (Read, Show)

type Grid = [[L]]

parse :: [String] -> Grid
parse = map $ map $ read . (:[])

main = print . count . parse . lines =<< readFile "input.txt"

transpose :: [[a]] -> [[a]]
transpose g = [[r!!n | r <- g] | n <- [0..length(head g)-1]]

count :: Grid -> Int
count g = count' allRows where
    allRows = concat [g, cols g, diag g, rev_diag g]
    cols = transpose
    diag = map catMaybes . transpose . pad
    rev_diag = diag . map reverse
    count' = sum . map nXMAS

pad :: [[a]] -> [[Maybe a]]
pad g = [replicate n Nothing ++ map Just row ++ replicate (size-n) Nothing
        | (row,n) <- zip g [0..]]
        where size = max (length g) $ length (head g)

nXMAS :: [L] -> Int
nXMAS (X:M:A:S:res) = 1 + nXMAS (S:res)
nXMAS (S:A:M:X:res) = 1 + nXMAS (X:res)
nXMAS []            = 0
nXMAS (_:res)       = nXMAS res

