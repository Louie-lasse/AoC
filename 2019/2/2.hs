type Code = [Int]

main :: IO (Int,Int)
main = do
    code <- map read . split <$> readFile "text.txt"
    return $ findVals code 0 0

findVals :: Code -> Int -> Int -> (Int, Int)
findVals code 100 n2  = findVals code 0 (n2+1)
findVals _    _   100 = error "no solution found"
findVals code n1 n2 | n1 <= 99 = valid 19690720 code n1 n2 ? (n1,n2) $ findVals code (n1+1) n2

valid :: Int -> Code -> Int -> Int -> Bool
valid v (_:_:_:r) i1 i2 = v == head (runCode 0 (1:i1:i2:r))

runCode :: Int -> Code -> Code
runCode pc code | 1 <-  code !! pc = runCode (pc + 4) $ runOp (+) pc code
                | 2 <-  code !! pc = runCode (pc + 4) $ runOp (*) pc code
                | 99 <- code !! pc = code
                | otherwise = error $ show code

runOp :: (Int -> Int -> Int) -> Int -> Code -> Code
runOp op pc code | (_:i1:i2:i3:_) <- drop pc code = setAt (op (val i1) (val i2)) i3 code
                 where
                    val = (!!) code
runOp _ _ code = error $ show code

split :: String -> [String]
split s | (r,',':r2) <- break (==',') s = r : split r2
        | otherwise = [s]

setAt x i xs = take i xs ++ x : drop (i + 1) xs

(?) :: Bool -> a -> a -> a
(?) True  a _ = a
(?) False _ a = a