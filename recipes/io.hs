main = do 
  let name = "Jhon"
  putStrLn "Hello World"
  putStrLn $ "Hello " ++ name
  putStrLn $ show 12
  print $ 100 ** 10
  print $ sqrt 20
  print $ round 3.2
  print $ floor 3.8
  print $ ceiling 3.1
  print "add your name"

  name <- getLine

  putStrLn $ "OK " ++ name ++ ". How old are you."
  age <- getLine
  let ageAsNumber = read age :: Int
  print ageAsNumber