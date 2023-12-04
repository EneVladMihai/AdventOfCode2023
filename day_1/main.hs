import System.Environment
import Data.Char (isDigit)
import Data.List (isPrefixOf)

-- Part 1
readLineNumber :: [Char] -> Int
readLineNumber line = read $ [head digits] ++ [last digits] where
    digits = filter isDigit line

-- Part 2
digits :: [([Char], [Char])] =
    [("one", "1"),
    ("two", "2"),
    ("three", "3"),
    ("four", "4"),
    ("five", "5"),
    ("six", "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("nine", "9")]

translateDigits :: [Char] -> [Char]
translateDigits [] = []
translateDigits line = newLine where
    digitsFound = filter (\(str, _) -> isPrefixOf str line) digits
    newLine = 
        if length digitsFound == 0 then
            head line : translateDigits (tail line)
        else
            digit ++ translateDigits (tail line)
            where (str, digit) = head digitsFound


main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    let calibrationValues = readLineNumber . translateDigits <$> rows
    print $ sum calibrationValues