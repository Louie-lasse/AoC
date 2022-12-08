data Item = Dir Dir | File Integer
type Dir  = [Item]

main :: IO ()
main = do
    lns <- lines <$> readFile "text.txt"
    let (dir,_) = parseLine lns
    print $ sum $ filter (<= 100000) $ sizes dir

parseLine :: [String] -> (Item, [String])
parseLine []                               = (Dir [], [])  --Base case
parseLine (('d':_):ss)                     = parseLine ss  --only proves that a dir exists
parseLine (('$':' ':'c':'d':' ':'.':_):ss) = (Dir [], ss)  --cd up, end this directory
parseLine (('$':' ':'l':_):ss)             = parseLine ss  --ls, can be ignored
parseLine (('$':' ':'c':_):ss)             = (Dir (subDir : dir), rest) -- read subdirectory
    where
        (subDir,  txt) = parseLine ss
        (Dir dir, rest) = parseLine txt
parseLine (s:ss) = (Dir $ File (read size) : dir, rest) --read file
    where
        (size, ' ':_)   = break (==' ') s
        (Dir dir, rest) = parseLine ss

sizes :: Item -> [Integer]
sizes (Dir d) = sizes' d 0
    where
        sizes' [] a = [a]
        sizes' (File i:r) a = sizes' r (i+a)
        sizes' (dir   :r) a = sizes' r (a + maximum (sizes dir)) ++ sizes dir
