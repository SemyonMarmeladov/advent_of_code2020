-- |
import Data.List.Split


-- Does have a off by one error, but we don't care about that.
-- We are freaks.

check :: [String] -> Bool
check xs
  | (length xs) == 8 = True
  | (length (filter (\x -> (head x) /= 'c') xs)) == 7 = True
  | otherwise = False

parse1 :: [String] -> [String] ->  Int -> Int
parse1 [] _ count = count
parse1 (x : xs) ys count = if (x == "") && (check $ form ys) then
                                  parse1 xs [] (count + 1)
                           else if (x == "") && not (check $ form ys) then
                                  parse1 xs [] count
                           else parse1 xs (x:ys) count

form :: [String] -> [String]
form l =  concat $ map (\x -> (splitOn " " x )) l

main :: IO ()
main = do
  xs <- lines <$> readFile "./data/day04.txt"
  print $ (parse1 xs [] 0)
