{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Day2 where

import GetInput (getFile)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Text (strip, pack, unpack)

data Color = Green | Red | Blue
    deriving (Show, Eq)
type Round = [(Color, Int)]

day2 :: IO()
day2 = do
    inp <- getFile "day2"
    let preparsed = map (splitOn ":") $ lines inp
    let parsed = map parseGame preparsed
    print $ part1 parsed
    print $ part2 parsed

part1 :: [(Int, [Round])] -> Int
part1 = sumGame

part2 :: [(Int, [Round])] -> Int
part2 = powerGame

parseGame :: [String] -> (Int, [Round])
parseGame (id:vals:[]) = (gameId, readRounds)
    where
        gameId = (read $ (splitOn " " id) !! 1) :: Int
        rounds = splitOn "; " vals
        readRounds = map parseRound rounds :: [Round]
parseGame xs = undefined

parseRound :: String -> Round
parseRound s = parsed
    where
        stripped = (unpack . strip . pack) s
        colors = map (splitOn " ") (splitOn ", " stripped)
        parsed = map (\x -> (getColor (last x), read (head x))) colors
        getColor str
            | "green" `isInfixOf` str = Green
            | "blue" `isInfixOf` str = Blue
            | "red" `isInfixOf` str= Red
            | otherwise = undefined

sumGame :: [(Int, [Round])] -> Int
sumGame [] = 0
sumGame ((id, rounds):res)
    | all validRound rounds = id + sumGame res
    | otherwise = sumGame res

powerGame :: [(Int, [Round])] -> Int
powerGame xs = sum $ map powerSet xs
    where
        powerSet :: (Int, [Round]) -> Int
        powerSet (_, rs) = r * g * b
            where
                (r,g,b) = minimalSet rs

minimalSet :: [Round] -> (Int, Int, Int)
minimalSet round = (maxValue r, maxValue g, maxValue b)
    where
        r = filter ((Red ==) . fst) $ concat round
        g = filter ((Green ==) . fst) $ concat round
        b = filter ((Blue ==) . fst) $ concat round
        maxValue :: Round -> Int
        maxValue [] = 0
        maxValue xs = maximum $ map snd xs

validRound :: Round -> Bool
validRound [] = True
validRound ((c, val):xs)
    | c == Red && val > 12 = False
    | c == Green && val > 13 = False
    | c == Blue && val > 14 = False
    | otherwise = validRound xs