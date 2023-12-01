module Main where

import Day1 (day1)


main :: IO ()
main = do
    input <- readFile "input/day1.txt"
    let res = day1 input
    print res
