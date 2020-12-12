-- |
import Data.List

check :: [Int] -> [Int] -> Int -> Maybe Int
check [] _ _ = Nothing
check (x:xs) lst i = case (find (\z -> z == i) (map (\a -> a + x) lst)) of
                        (Just b) -> Just b
                        Nothing -> check xs lst i

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

part1 :: [Int] -> Int -> Int
part1 lst start  = case (check l l (lst !! start)) of
                 (Just x) -> part1 lst (start+1)
                 Nothing  -> (lst !! start)
                 where l = (slice (start - 25) (start - 1) lst)

part2 :: [Int] ->

main :: IO ()
main = do
  xs <-  fmap (read::String->Int) <$> lines <$> readFile "./data/day9.txt"
  print (part1 xs 25)
