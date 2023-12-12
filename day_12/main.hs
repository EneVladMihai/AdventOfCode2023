import System.Environment
import Data.Text (splitOn, pack, unpack)
import Data.List (isPrefixOf, intercalate)

-- Part 1
replaceIdx :: String -> Int -> String -> String
replaceIdx str idx replace = take idx str ++ replace ++ drop (idx+1) str

springGroups :: [Char] -> [Int]
springGroups [] = []
springGroups str = if groupLen>0 then groupLen:springGroups rest else springGroups rest
    where
        skipDots = dropWhile (=='.') str
        groupLen = length $ takeWhile (=='#') skipDots
        rest = drop groupLen skipDots

isValid :: [Int] -> String -> Bool
isValid [] str = '#' `notElem` str
isValid (l:rl) str = groupLen == l && isValid rl remainingStr where
    skipDots = dropWhile (=='.') str
    groupLen = length $ takeWhile (=='#') skipDots
    remainingStr = drop groupLen skipDots
    
solve :: Int -> (String, [Int]) -> [String]
solve charIdx (str, condition)
    | not $ isValid2 condition str = []
    | length str <= charIdx = [str]
    | otherwise = if (str !! charIdx) == '?'
        then concatMap (\c -> solve (charIdx+1) (replaceIdx str charIdx c, condition)) [".", "#"]
        else solve (charIdx+1) (str, condition)

parse :: String -> (String, [Int])
parse row = (springs, conditions) where
    [springs, unparsedConditions] = words row
    conditions = read.unpack <$> splitOn (pack ",") (pack unparsedConditions)

part1 :: [(String, [Int])] -> Int
part1 = sum . fmap (length.solve 0)

-- Part 2
parse2 :: String -> (String, [Int])
parse2 s = (intercalate "?" (replicate 5 springs), concat (replicate 5 conditions)) 
    where (springs, conditions) =  parse s

-- Used to do early prunning
isValid2 :: [Int] -> String -> Bool
isValid2 conds str
    | noAmbiguity == str = groups == conds
    | noAmbiguity == "" = True
    | length groups > length conds = False
    | noAmbiguity == str = groups `isPrefixOf` conds
    | last noAmbiguity == '#' = (init groups `isPrefixOf` conds) 
                                && (last groups <= (conds !! (length groups - 1)))
    | last noAmbiguity == '.' = groups `isPrefixOf` conds 
    where
        noAmbiguity = takeWhile (/='?') str
        groups = springGroups noAmbiguity

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    print $ part1 $ parse <$> rows
    print $ part1 $ parse2 <$> rows