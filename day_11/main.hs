{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
import System.Environment

type Galaxy = (Int, Int)

indexed :: [b] -> [(Int, b)]
indexed = zip [0..]

parseGalaxies :: [(Int, String)] -> [Galaxy]
parseGalaxies [] = []
parseGalaxies (row:rs) =
    ((\col -> (fst row, fst col)) <$> filter ((=='#').snd) (indexed (snd row)))
    ++ parseGalaxies rs

transpose :: [String] -> [String]
transpose rows = (\(colHeadIdx, _) -> map (!! colHeadIdx) rows) <$> indexed (head rows)

isEmpty :: String -> Bool
isEmpty = notElem '#'

expand :: [String] -> [String]
expand = foldl (\acc row -> if isEmpty row then (replicate 2 row) ++ acc else row:acc) []

expand2D :: [String] -> [String]
expand2D = transpose.expand.transpose.expand

distance :: Galaxy -> Galaxy -> Int
distance (x1 , y1) (x2 , y2) = abs (x1-x2) + abs (y1-y2)

part1 :: [Galaxy] -> Int
-- The list comp has each pair twice
part1 galaxies = (`div` 2) $ sum (uncurry distance <$> [(g1, g2) | g1 <- galaxies, g2 <- galaxies, g1 /= g2])


-- Part 2

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    let expanded = expand2D rows
    let galaxies = parseGalaxies.indexed $ expanded
    print $ part1 galaxies

