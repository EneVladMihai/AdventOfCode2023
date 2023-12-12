import System.Environment
import Data.Text (splitOn, pack, unpack)
import Data.List (isPrefixOf, intercalate)
import Control.Arrow ((***))
import Data.MemoTrie (memo)

parse :: String -> (String, [Int])
parse row = (springs, conditions) where
    [springs, unparsedConditions] = words row
    conditions = read.unpack <$> splitOn (pack ",") (pack unparsedConditions)

parse2 :: String -> (String, [Int])
parse2 = (intercalate "?" . replicate 5 *** concat . replicate 5).parse

-- The memoization caused some headaches to get running with
-- not really sure how it works with external packages for haskell
solve :: (String, [Int]) -> Int
solve = memo solve'

solve' :: (String, [Int]) -> Int
solve' (str, conditions)
    | null str = if null conditions then 1 else 0
    | length str < (sum conditions + length conditions - 1) = 0
    | otherwise = case head str of
       '.' -> solve (dropWhile (=='.') str, conditions)
       '?' -> solve ('.':tail str, conditions) + solve ('#':tail str, conditions)
       '#'
            | null conditions -> 0
            | elem '.' $ take (head conditions) str -> 0
            | length str == head conditions -> solve([], tail conditions)
            | str !! head conditions == '#' -> 0
            | otherwise -> solve(drop (head conditions + 1) str, tail conditions)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    let result = sum . fmap solve
    print $ result $ parse <$> rows
    print $ result $ parse2 <$> rows