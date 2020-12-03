partOne :: [String] -> Int -> Int -> Int -> Int
partOne [] n _ _  = n
partOne (x:xs) n index step = if (x !! ((index) `mod ` 31)) == '#' then
                         partOne xs (n + 1) (index + step) step
                         else partOne xs n (index + step) step

partTwo :: [String] -> Int -> Int -> Int
partTwo [_] n _= n
partTwo [] n _ = n
partTwo (x:_:xs) n index = if (x !! ((index) `mod ` 31)) == '#' then
                         partTwo xs (n + 1) (index + 1)
                         else partTwo xs n (index + 1)

sols :: [String]-> Int
sols xs = let s = [(partOne xs 0 0 1), (partOne xs 0 0 3), (partOne xs 0 0 5), (partOne xs 0 0 7), (partTwo xs 0 0)] in
                foldr (*) 1 s

main = do
  xs <- lines <$> readFile "./data/day03.txt"
  print (partTwo xs 0 0 )
  print (sols xs)
