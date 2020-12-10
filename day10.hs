-- |


check :: [Int] -> [Int] -> Int -> Maybe
check [] =
check (x:xs) =


main :: IO ()
main = do
  xs <- lines . readFile "./data/day10.txt"
  print xs
