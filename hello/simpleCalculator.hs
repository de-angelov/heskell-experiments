main :: IO ()
main = do
  putStrLn "Enter number"
  str1 <- getLine
  putStrLn "Enter an operator: "
  operator <- getLine
  putStrLn "Enter second number"
  str2 <- getLine
  let 
    firstNumber = read str1 :: Double
    secondNumber = read str2 :: Double
    result = case operator of
      "*" -> firstNumber * secondNumber
      "/" -> firstNumber / secondNumber
      "+" -> firstNumber + secondNumber
      "-" -> firstNumber - secondNumber
      _ -> -1
  print result

