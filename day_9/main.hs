import System.Environment
parse :: String -> [Int]
parse = fmap read . words

diffs l = uncurry (-) <$> zip (drop 1 l) (take (length l-1) l )

fullbreakDown :: [Int] -> [[Int]]
fullbreakDown [0] = [[0]]
fullbreakDown l = l:fullbreakDown (diffs l)

part1 l = sum $ last <$> fullbreakDown l

part2 l = foldr1 (-) $ head <$> fullbreakDown l

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = parse <$> lines content

    print $ sum $ part1 <$> rows
    print $ sum $ part2 <$> rows
