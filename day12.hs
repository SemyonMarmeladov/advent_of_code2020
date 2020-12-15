-- |

data Instruction = North | East | West | South | Lef | Righ | Forward
  deriving (Show)


parseD :: String -> (Instruction, Int)
parseD ('L':xs) = (Lef, read xs :: Int)
parseD ('R':xs) = (Righ, read xs :: Int)
parseD ('S':xs) = (South, read xs :: Int)
parseD ('W':xs) = (West, read xs :: Int)
parseD ('E':xs) = (East, read xs :: Int)
parseD ('N':xs) = (North, read xs :: Int)
parseD ('F':xs) = (Forward, read xs :: Int)


-- a north, b south, c east, d west

part1 :: [String] -> (Int,Int,Int,Int) -> Int -> Int
part1 [] (a,b,c,d) direc = (abs (a - b)) + (abs (c - d))
part1 (x:xs) (a,b,c,d) direc = case (parseD x) of
  (North, x) -> part1 xs ((a+x), b,c,d) direc
  (South, x) -> part1 xs (a, (b + x),c,d) direc
  (East, x) -> part1 xs (a, b,(c + x),d) direc
  (West, x) -> part1 xs (a, b,c,(d + x)) direc
  (Lef, x) -> part1 xs (a, b, c, d) ((direc - x) `mod` 360)
  (Righ, x) -> part1 xs (a, b,c,d) ((direc + x) `mod` 360)
  (Forward, x) -> part1 xs (dAdd (a,b,c,d) direc x) direc

dAdd :: (Int, Int, Int, Int) -> Int -> Int -> (Int,Int,Int,Int)
dAdd (a,b,c,d) direc x
 | direc == 90 = (a, b,(c + x),d)
 | direc == 180 = (a, (b +x),c, d)
 | direc == 270 = (a, b ,c,(d +x))
 | direc == 360 = ((a+x), b ,c,d)
 | otherwise = (a,b,c,(d + 3000000))

main :: IO ()
main = do
  xs <- lines <$> readFile "./data/day12.txt"
  print $ part1 xs (0,0,0,0) 90
