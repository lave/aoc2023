import Data.Char
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.String.Utils

import Common

main = do
    content <- readFile "01.input"
    let ls = lines content
    putStrLn $ show $ ls

    -- let s = sum $ map findDigits ls
    -- putStrLn $ show $ s
    -- putStrLn $ show $ map (\x -> (x, replaceDigits x, findDigits $ replaceDigits x)) ls

    let s_ = sum $ map findDigits $ ls
    putStrLn $ show $ s_

findDigit digits cs
    | c >= '0' && c <= '9' = (ord c) - (ord '0')
    | isJust f = snd $ fromJust f
    | otherwise = findDigit digits $ tail cs
    where
        c = head cs
        f = find (\(x, y) -> startswith x cs) digits

findDigits l = d1 * 10 + d2
    where
        digits = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]
        rdigits = map (\(x, y) -> (reverse x, y)) digits
        d1 = findDigit digits l
        d2 = findDigit rdigits $ reverse l

findDigits_ l = d1 * 10 + d2
    where
        digits = filter (\c -> c >= '0' && c <= '9') l
        d1 = (ord $ head digits) - (ord '0')
        d2 = (ord $ last digits) - (ord '0')
    
replaceDigits l =    
    foldl (\s (a, b) -> replace a b s) l [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]
