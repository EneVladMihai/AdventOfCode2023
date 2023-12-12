import System.Environment
import Data.Text (splitOn, pack, unpack)
import Data.List (isPrefixOf, intercalate)
import Control.Arrow ((***))
import Data.MemoTrie (memo)

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
parse2 :: String -> (String, [Int])
parse2 = (intercalate "?" . replicate 5 *** concat . replicate 5).parse

-- The memoization caused some headaches to get running with
-- not really sure how it works with external packages for haskell
solve2 :: (String, [Int]) -> Int
solve2 = memo solve2'

solve2' :: (String, [Int]) -> Int
solve2' (str, conditions)
    | null str = if null conditions then 1 else 0
    | length str < (sum conditions + length conditions - 1) = 0
    | otherwise = case head str of
       '.' -> solve2 (dropWhile (=='.') str, conditions)
       '?' -> solve2 ('.':tail str, conditions) + solve2 ('#':tail str, conditions)
       '#'
            | null conditions -> 0
            | elem '.' $ take (head conditions) str -> 0
            | length str == head conditions -> solve2([], tail conditions)
            | str !! head conditions == '#' -> 0
            | otherwise -> solve2(drop (head conditions + 1) str, tail conditions)

part2 :: [(String, [Int])] -> Int
part2 = sum . fmap solve2

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    print $ part2 $ parse <$> rows
    print $ part2 $ parse2 <$> rows