import Data.List
-- input is a sorted list
part1 :: [Int] -> (Int, Int) -> Int
part1 [] (a,c) = a * c
part1 [_] (a,c) = (1+a) * c
part1 (x:y:xs) (a,c)
  | (x + 1) == (y) = part1 xs ((a+1), c)
  | (x + 3) == (y) = part1 xs (a, (c+1))
  | otherwise = part1 xs ((a +100),c)


count :: Eq a => a -> [a] -> Int
count c = length . filter (==c)

f :: [Int] -> Int
f xs = get1diffs * get3diffs
  where
    getdiffs = zipWith (-) (tail xs) xs 
    get1diffs = count 1 getdiffs
    get3diffs = count 3 getdiffs


main :: IO ()
main =
  do
    xs <- fmap (read::String ->Int) <$> lines <$> readFile "./data/day10.txt"
    print $ f (0:(sort xs) ++ [(maximum xs + 3)])
