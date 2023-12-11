{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
import System.Environment
import Data.List (sort)
import Control.Monad (liftM2)

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
part1 galaxies = (`div` 2) $ sum (uncurry distance <$> gPairs) 
    where gPairs = [(g1, g2) | g1 <- galaxies, g2 <- galaxies, g1 /= g2]

-- Part 2
-- In part 1 we took the navie approach of actually expanding the space and then calculating
-- which is no longer feasible for part to. We can take the expansion into account when calculating distances
-- i.e. how many rows & cols between the two galaxies have expanded
type Expansion = Int

getExpandedIdx :: [String] -> [Int]
getExpandedIdx = map fst . filter (\(_, v) -> isEmpty v) . indexed

-- Trick to allow us combine two predicates
-- works because a predicate x -> Bool is a Monad where x is the wrapped value
(.&&.) = liftM2 (&&)

distanceWithExpansions :: Int -> [Expansion] -> [Expansion] -> Galaxy -> Galaxy -> Int
distanceWithExpansions f expx expy (x1 , y1) (x2 , y2) = x2'-x1' + dx*(f-1) + y2'-y1' + dy*(f-1)
    where 
        [x1', x2'] = sort [x1, x2]
        [y1', y2'] = sort [y1, y2]
        dx = length $ filter ((>x1') .&&. (<x2')) expx
        dy = length $ filter ((>y1') .&&. (<y2')) expy

part2 :: Int -> [Expansion] -> [Expansion] -> [Galaxy] -> Int
-- The list comp has each pair twice
part2 f expx expy galaxies = (`div` 2) $ sum (distFunc <$> gPairs) 
    where 
        gPairs = [(g1, g2) | g1 <- galaxies, g2 <- galaxies, g1 /= g2]
        distFunc = uncurry (distanceWithExpansions f expx expy)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    -- let expanded = expand2D rows
    -- let galaxies = parseGalaxies.indexed $ expanded
    -- print $ part1 galaxies

    let galaxies = parseGalaxies.indexed $ rows
    let (rowExpansions, colExpansions) = (getExpandedIdx rows, (getExpandedIdx.transpose) rows)
    print $ part2 1000000 rowExpansions colExpansions galaxies

