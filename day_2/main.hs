import System.Environment
import Data.List (isPrefixOf)

-- Part 1
parse :: [String] -> [(Int, [String])]
parse = zip [1..] . fmap (drop 2 . words)

toInt :: String -> Int
toInt = read

isValid :: [String] -> Bool
isValid [] = True
isValid (val:color:rest)
    | "blue" `isPrefixOf` color    = toInt val <= 14 && isValid rest
    | "green" `isPrefixOf` color   = toInt val <= 13 && isValid rest
    | "red" `isPrefixOf` color     = toInt val <= 12 && isValid rest

part1 :: [(Int, [String])] -> Int
part1 games = result where
    result = sum $ fst <$> validRows
    validRows = filter (isValid . snd) games


-- Part 2
colorValues :: [String] -> [[Int]]
colorValues [] = [[], [], []]
colorValues (val:color:rest)
    | "blue" `isPrefixOf` color    = [v:blue, green, red]
    | "green" `isPrefixOf` color   = [blue, v:green, red]
    | "red" `isPrefixOf` color     = [blue, green, v:red]
    where
        v = toInt val
        [blue, green, red] = colorValues rest

part2 :: [(Int, [String])] -> Int
part2 games = result where
    result = sum powers
    powers = product . fmap maximum . colorValues . snd <$> games

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    print $ (part1 . parse) rows
    print $ (part2 . parse) rows