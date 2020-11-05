scores :: [Int]
scores = [19, 83, 100]

main = do 
  print $ scores !! 0
  print $ head scores
  print $ last scores
  print $ init scores
  print $ tail scores