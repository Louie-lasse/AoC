import Data.Functor
import Data.Char
import Data.List
main = do
    readFile "input.txt" >>= print . length . paths "start" "end" . removeStart . createGraph . map parsePair . lines

type Node = String

type Edge = (Node,Node)

type Graph = [(Node,[Node])]

type Map = [(Node,Bool)]

parsePair :: String -> Edge
parsePair s | (f,'-':l) <- break (=='-') s = (f,l)
            | otherwise                    = error $ "failed to parse string " ++ s

createGraph :: [Edge] -> Graph
createGraph es = createGraph' es [] where
    createGraph' [] g = g
    createGraph' (e:es) g = createGraph' es $ swap e `add` (e `add` g)

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

add :: Edge -> Graph -> Graph
add (a,b) []                  = [(a,[b])]
add (a,b) ((f,t):es) | a == f = (f,b:t):es
                     | otherwise = (f,t) : add (a,b) es

createMap :: Graph -> Map
createMap []         = []
createMap ((f,_):ns) = (f,True) : createMap ns

paths :: Node -> Node -> Graph -> [[Node]]
paths s e g = paths' s e True g (crossOut "start" $ createMap g) where
    paths' :: Node -> Node -> Bool -> Graph -> Map -> [[Node]]
    paths' c e b g m | c == e = [[e]]
                     | otherwise = useExtra ++ notUseExtra  --concatMap (\n -> paths' n e g newMap) next where
                       where
                        notUseExtra = map (c :) $ concat [paths' n e b g newMap | n <- next, canWalkOn n m]
                        useExtra = map (c :) $ concat [paths' n e False g newMap | n <- next, canUseExtra n b m]
                        canUseExtra :: Node -> Bool -> Map -> Bool
                        canUseExtra n b m = b && isLower (head n) && not (canWalkOn n m)
                        newMap = crossOut c m
                        next | (Just ns) <- lookup c g = ns
                             | otherwise               = error $ "couldn't find "++c++" in the graph "++show g


crossOut :: Node -> Map -> Map
crossOut r []                     = error $ "couldn't find node "++r++" for 'crossOut'"
crossOut r ((n,b):ns) | isUpper $ head r = (n,b):ns
                      | r == n    = (n,False) : ns
                      | otherwise = (n,b)    : crossOut r ns

canWalkOn :: Node -> Map -> Bool
canWalkOn n m | (Just True) <- lookup n m = True
              | otherwise                 = False

removeStart :: Graph -> Graph
removeStart []          = []
removeStart ((n,cs):ns) = (n,remove' cs) : removeStart ns where
    remove' :: [Node] -> [Node]
    remove' []                    = []
    remove' (c:cs) | c == "start" = cs
                   | otherwise    = c : remove' cs