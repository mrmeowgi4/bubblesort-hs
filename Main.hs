actualSort :: [Int] -> [Int]
actualSort a = actualSort' a (length a)
  where
    actualSort' a 0 = a
    actualSort' a g = actualSort' (pass a) (g - 1)
    
    pass (x:y:xs)
      | x > y     = y : pass (x:xs)
      | otherwise = x : pass (y:xs)
    pass xs = xs

bubbleSort :: [Int] -> [Int]
bubbleSort a
  | null a    = a
  | otherwise = actualSort a

main :: IO ()
main = do
  let a = [1, 3, 0]
  print $ bubbleSort a
