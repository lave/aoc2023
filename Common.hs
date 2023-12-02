module Common where
import qualified Data.Set
import qualified Data.Either

readSingleLineFile filename = do
    content <- readFile filename
    let [line] = lines content
    return line

readInt s = read s :: Int

readsInt s = reads s :: [(Int, String)]

readsStr s = reads s :: [(String, String)]

readsChar [] = []
readsChar (' ' : s) = readsChar s
readsChar (c : s) = [(c, s)]

maybeDo r s =
    case res of
        [] -> [(Nothing, s)]
        [(r', s')] -> [(Just r', s')]
    where
        res = case s of
            [] -> []
            otherwise -> r s

firstDup l = firstDup' l Data.Set.empty 0
    where
        firstDup' [] _ _ = Nothing
        firstDup' (x : xs) xs' i
            | Data.Set.member x xs' = Just (i, x)
            | otherwise = firstDup' xs (Data.Set.insert x xs') (i + 1)

fst3 (x, _, _) = x
snd3 (_, y, _) = y
trd3 (_, _, z) = z

fromLeft_ = Data.Either.fromLeft undefined
fromRight_ = Data.Either.fromRight undefined
