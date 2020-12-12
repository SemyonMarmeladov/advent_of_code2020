

part1 :: [Int] -> (Int, Int, Int) -> Int
part xs ts =

main :: IO ()
main =
  do
    xs <- fpa (read::String ->Int) <$> lines <$> readFile "./data/day10"
    xs <- part1 xs (0,0,0)
