import Data.Char

test :: [String]
test = ["9-11", "p:", "pppppppppppppplx"]

test1 :: [[String]]
test1 = ["3-10", "p:", "ppppp"] : []

getMinMax' :: String -> String -> (String, String)
getMinMax' (x:xs) mmin 
  | x == '-' = (mmin, xs) 
  | (isNumber x) = getMinMax' xs (x : mmin)

minMax :: String -> (Int, Int)
minMax x = let r = (getMinMax' x []) in
         (read (fst r) :: Int, read (snd r) :: Int)

getMin :: String -> Int
getMin x = (fst (minMax x))

getMax :: String -> Int
getMax x = (snd (minMax x))
 
filterChars :: Int -> Int -> Char ->  String -> Bool
filterChars max min l s = 
  let st = filter (\x -> x == l) s in
    ((length st) <= max && (length st) >= min)

sol' :: [[String]] -> [[String]]
sol' x = filter (\x -> filterChars (getMax (x !! 0)) (getMin (x !! 0))
                                 (head (x !! 1)) (x !! 2)) x
sol :: [[String]] -> Int
sol xs = length $ sol' xs

main :: IO ()
main = do 
  xs <- lines <$> readFile "./data/day2.txt" 
  print $ (sol' (map words xs))
