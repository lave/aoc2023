import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String.Utils

import Common

main = do
    content <- readFile "02.input"
    let games = map parse $ lines content
    --putStrLn $ show $ games

    let possible = filter (isPossible (12, 13, 14)) games
    --putStrLn $ show $ possible
    let n = sum $ map fst possible
    putStrLn $ show $ n

    let mins = map minCubes games
    --putStrLn $ show $ mins
    let n = sum $ map snd mins
    putStrLn $ show $ n


parse = head . parseLine
    where
        parseLine s = [(id, parseGame s3) |
            ("Game", s1) <- lex s,
            (id, s2) <- readsInt s1,
            (':', s3) <- readsChar s2]
        parseGame s = map parseDraw $ splitOn ";" s
        parseDraw s = (r, g, b)
            where
                colors = map parseColor $ splitOn "," s
                r = fromMaybe 0 $ fmap snd $ find (\c -> fst c == "red") colors
                g = fromMaybe 0 $ fmap snd $ find (\c -> fst c == "green") colors
                b = fromMaybe 0 $ fmap snd $ find (\c -> fst c == "blue") colors
        parseColor s = (color, read n :: Integer)
            where
                [n, color] = words s

isPossible limits (_, draws) = all id $ map (isPossible' limits) draws

isPossible' (r_, g_, b_) (r, g, b) = r <= r_ && g <= g_ && b <= b_

minCubes (id, draws) = (id, r * g * b)
    where
        r = maximum (map fst3 draws)
        g = maximum (map snd3 draws)
        b = maximum (map trd3 draws)

