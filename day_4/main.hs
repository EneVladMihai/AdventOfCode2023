import System.Environment
import Data.List (break, dropWhileEnd)
import Data.List.NonEmpty (unfold, unfoldr)

-- Part 1
trim :: Eq a => a -> [a] -> [a]
trim char = dropWhileEnd (== char) . dropWhile (== char)

split :: Char -> String -> [String]
split _ [] = [""]  -- Empty string
split separator (c:cs)
    | c == separator && head rest == "" = rest
    | c == separator = "" : rest
    | otherwise = (c : head rest) : tail rest
    where rest = split separator cs 

readNumbers :: Int -> [Char] -> ([Int], [Int])
readNumbers dropFirstN line = (winningNumbers, myNumbers) where
    [leftNumbers, rightNumbers] = split '|' $ drop dropFirstN line
    splitTrim = split ' ' . trim ' '
    winningNumbers = read <$> splitTrim leftNumbers :: [Int]
    myNumbers = read <$> splitTrim rightNumbers :: [Int]

getMatches :: ([Int], [Int]) -> Int
getMatches (winningNumbers, myNumbers) = length $ filter (\wn -> elem wn myNumbers) winningNumbers

getScore :: Int -> Int
getScore matches = if matches > 0 then 2^(matches-1) else 0

-- Part 2
-- This is Dynamic Programming in Haskell
-- dp[i] = 1 + all the copies (including child copies of copies and so on)
-- because of recursion we start with the last card dp[last] = 1
-- and build towards the first (as suggested by the fact that we win copies below the current card)
-- we'll have to sum all the values in dp to account for each card and their tree of copies
getCopies :: [Int] -> [Int]
getCopies [] = []
getCopies (matches:xs) = 1 + sum(take matches dp) : dp where
    dp = getCopies xs


main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content
    let numberOfCharsToSkipEachLine = read $ args !! 1 -- Skips the 'Card  xx: ' part

    let numbers = readNumbers numberOfCharsToSkipEachLine <$> rows
    let scores = getScore . getMatches <$> numbers
    print $ sum scores

    let totalScracthcards = sum $ getCopies $ getMatches <$> numbers
    print totalScracthcards