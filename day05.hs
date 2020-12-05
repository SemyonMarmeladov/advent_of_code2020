import Data.List

solve :: String -> [Int] -> Int 
solve _ [x] = x
solve [] xs = 0
solve (x:xs) l 
  | x == 'F' = solve xs (take (length l `div` 2) l)
  | x == 'B' = solve xs (drop (length l `div` 2) l) 
  | x == 'R' = solve xs (drop (length l `div` 2) l) 
  | x == 'L' = solve xs (take (length l `div` 2) l) 
  | otherwise = solve xs (drop (length l `div` 2) l) 

solver :: [String] -> Int -> Int  
solver [] max = max 
solver (x : xs) max = 
  let id = (solve (take 7 x) [0..127]) * 8 + 
           (solve (drop 7 x) [0..7]) in

  if (id > max) then solver xs id
  else solver xs max

cnc :: [String] -> [Int] 
cnc = sort . map (\x -> (solve (take 7 x) [0..127]) * 8 + 
           (solve (drop 7 x) [0..7])) 

spotFinder :: [Int] -> Int
spotFinder [] = 0
spotFinder (x:y:xs)  
  | (x + 1) /= y = (x + 1)
  | otherwise = (spotFinder xs)

main :: IO ()
main = do 
  xs <- lines <$> readFile "./data/day05.txt"
  print $ (cnc xs)
