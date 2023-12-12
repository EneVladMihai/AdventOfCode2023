import System.Environment
import Data.Text (splitOn, pack, unpack)


-- Part 1
replaceIdx :: String -> Int -> String -> String
replaceIdx str idx replace = take idx str ++ replace ++ drop (idx+1) str

springGroups str = (takeWhile (=='#') . dropWhile (=='.') $ str) : springGroups str

isValid :: [Int] -> String -> Bool
isValid [] str = '#' `notElem` str
isValid (l:rl) str = groupLen == l && isValid rl remainingStr where
    skipDots = dropWhile (=='.') str
    groupLen = length $ takeWhile (=='#') skipDots
    remainingStr = drop groupLen skipDots

solve :: Int -> (String, [Int]) -> [String]
solve charIdx (str, condition)
    | not $ isValid condition str = []
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

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    -- print $ solve 0.parse.last $ rows
    print $ part1 $ parse <$> rows