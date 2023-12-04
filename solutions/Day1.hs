module Day1 where

import Data.Char
import GetInput (getFile)

day1 :: IO ()
day1 = do
    s <- getFile "day1"
    let (p1, p2) = (day1p1 s, day1p2 s)
    print p1
    print p2

day1p1 :: String -> Int
day1p1 s = sum numbers
    where
        input = lines s
        numbers = [read $ (getFirstDigit x) : [(getFirstDigit (reverse x))] | x <- input]
getFirstDigit :: String -> Char
getFirstDigit [] = 'z'
getFirstDigit (c:s) 
    | isDigit c = c
    | otherwise = getFirstDigit s


day1p2 :: String -> Int
day1p2 s = sum numbers
    where
        input = lines s
        parsed = map preParse input
        numbers = [read [(getFirstDigit x), (getFirstDigit (reverse x))] | x <- parsed]

preParse :: String -> String
preParse [] = []
preParse ('o':'n':'e':s) = '1' : preParse ("ne" ++ s)
preParse ('t':'w':'o':s) = '2' : preParse ("wo" ++ s)
preParse ('t':'h':'r':'e':'e':s) = '3' : preParse ("hree" ++ s)
preParse ('f':'o':'u':'r':s) = '4' : preParse ("our" ++ s)
preParse ('f':'i':'v':'e':s) = '5' : preParse ("ive" ++ s)
preParse ('s':'i':'x':s) = '6' : preParse ("ix" ++ s)
preParse ('s':'e':'v':'e':'n':s) = '7' : preParse ("even" ++ s)
preParse ('e':'i':'g':'h':'t':s) = '8' : preParse ("ight" ++ s)
preParse ('n':'i':'n':'e':s) = '9' : preParse ("ine" ++ s)
--preParse ('z':'e':'r':'o':s) = '0' : preParse s
preParse (c:s) = c : preParse s
