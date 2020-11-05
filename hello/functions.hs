data Student = Student 
  { studentAge :: Int
  , studentGpa :: Double
  , studentMajor :: String
  , studentName :: String
  } deriving (Show)
 

travelToWork :: String -> Bool -> IO ()
travelToWork weather isRaining = do
  if weather == "cloudy" && isRaining 
  then putStrLn "drive to work"
  else putStrLn "walk to work"

cube :: Int -> Int
cube num = num * num * num

sayHi :: IO ()
sayHi = do 
  putStrLn "do stuff 1"
  putStrLn "welcome!"

main :: IO ()
main = do
  let student = Student 
                { studentName = "Alex Smith"
                , studentMajor = "Architecture"
                , studentAge = 19
                , studentGpa = 5.0
                }
              
  print $ show student
  putStrLn $ studentName student