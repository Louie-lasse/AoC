{-# LANGUAGE TupleSections #-}
import Data.Functor ((<&>))
import Data.Char (isDigit)
import qualified Data.List
import Data.Bifunctor (second, first)
import Data.Maybe (mapMaybe)

type Pos = (Int, Int)

data Val = Null | Val Int | Sym
val (Val i) = i
val _       = error "value of non value type"

type Map = [[Val]]

(!!!) :: [[a]] -> Pos -> Maybe a
m !!! (x,y) | (_,row:_) <- y `splitAt` m
            , (_,a  :_) <- x `splitAt` row
            = Just a
            | otherwise = Nothing

disp :: Val -> Char
disp Null = ' '
disp (Val n) = head $ show n
disp Sym     = '@'

isSym :: Val -> Bool
isSym Sym = True
isSym _   = False

parseVal :: Char -> Val
parseVal '.' = Null
parseVal c   | isDigit c = Val $ read [c]
             | otherwise = Sym

parseMap :: [String] -> Map
parseMap = map (map parseVal)

parseNum :: [Pos] -> Map -> Int
parseNum (p:ps) m | Just (Val i) <- m !!! p
                  = parseNum ps m + foldr (*) i (replicate (length ps) 10)
                  | otherwise = error $ "Non number at position "++show p
parseNum _      _ = 0

main = do --readFile "test.txt" <&> parseMap . lines >>= \m -> mapM_ (print . flip parseNum m) clusters m 
    m <- parseMap . lines <$> readFile "input.txt"
    let c = clusters m
    let nums = filter (hasSym m . adjacent) c
    print $ sum $ map (`parseNum` m) nums 

hasSym :: Map -> [Pos] -> Bool
hasSym m = any (hasSym' m) where
    hasSym' m p | Just Sym <- m !!! p = True
                | otherwise           = False

adjacent :: [Pos] -> [Pos]
adjacent ps = (lowX-1,y)
            : (highX+1,y)
            : [(x,y') | x <- [lowX-1..highX+1], y' <- [y-1,y+1]]
    where
        lowX = minimum $ map fst ps
        highX = maximum $ map fst ps
        y = maximum $ map snd ps


clusters :: Map -> [[Pos]]
clusters =  filter (not . null) . clusters' (0,0) where
    clusters' :: Pos -> Map -> [[Pos]]
    clusters' pos m | highX pos m = clusters' (resetX pos) m
                    | highY pos m = [[]]
                    | Just (Val _) <- m !!! pos
                    , (c:cs) <- clusters' (inc pos) m
                    = (pos:c):cs
                    | otherwise = [] : clusters' (inc pos) m

highX :: Pos -> Map -> Bool
highX (x,y) m | (_,r:_) <- y `splitAt` m
              = x >= length r
              | otherwise = False

highY :: Pos -> Map -> Bool
highY (_,y) m = y >= length m

inc :: Pos -> Pos
inc = first (+1)

resetX :: Pos -> Pos
resetX = (0,) . (+1) . snd