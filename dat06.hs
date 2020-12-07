-- |
import Data.List

part1 :: [String] -> Int -> [String] -> Int
part1 [] n _ = n
part1 (x:xs) n acc
  | x == "" = (part1 xs (n + (count acc)) [])
  | otherwise = (part1 xs n (x:acc))
  where count x = length $ nub $ concat x

main :: IO ()
main = do
  xs <- lines <$> readFile "./data/data06.txt"
  print $ part1 xs 0 []
