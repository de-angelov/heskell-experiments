
{-# LANGUAGE  RecordWildCards #-}
module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), SpecialKey (KeyUp, KeyDown, KeySpace), Key (SpecialKey), KeyState (Down) )
import System.Random (Random (randomR), mkStdGen)

windowName = "My First Game"
windowSize = (640, 480)
windowPosition = (100, 100)
displayMode  = InWindow windowName windowSize windowPosition

backgroundColor :: Color
backgroundColor = white

data Position =  P1 | P2 | P3 | P4 | P5 
  deriving (Show, Eq)

class IncOrDec a where 
  increase :: a -> a
  decrease :: a -> a

class ToFloat a where 
  toFloat :: a -> Float

instance ToFloat Position where 
  toFloat P1 = 1
  toFloat P2 = 2
  toFloat P3 = 3
  toFloat P4 = 4 
  toFloat P5 = 5 

instance  IncOrDec Position where 
  increase P1 = P2
  increase P2 = P3
  increase P3 = P4
  increase P4 = P5
  increase P5 = P5

  decrease P5 = P4
  decrease P4 = P3
  decrease P3 = P2
  decrease P2 = P1
  decrease P1 = P1

data Direction = UP | DOWN 
  deriving (Show, Eq)

data GameState = GameState 
  { isGameOver :: Bool 
  , getDirection :: Direction 
  , getPosition :: Position 
  }

createInitialGameState :: Bool -> GameState
createInitialGameState gameOver = 
  let
    pseudoRandomGenerator = mkStdGen 1000
    roll = fst $ randomR (0 :: Int, 1:: Int) pseudoRandomGenerator
    position = if roll == 1 then UP else DOWN
  in GameState 
    { isGameOver = gameOver 
    , getDirection = position
    , getPosition = P3 
    }

changeDirection :: Direction -> GameState -> GameState
changeDirection direction gameState = gameState { getDirection = direction }


-- Number of simulation steps to take for each second of real time.
numberSteps = 2 

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gameState = changeDirection DOWN gameState 
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState = changeDirection UP gameState 
handleKeys (EventKey (SpecialKey KeySpace ) Down _ _) gameState@GameState { isGameOver = isOver, getPosition = pos} = 
  gameState 
  { isGameOver = not isOver
  , getPosition = if isOver then P3 else pos
  }
handleKeys _ gameState = gameState

checkGameOver :: GameState -> Bool 
checkGameOver GameState { getDirection = DOWN, getPosition= P5 } = True 
checkGameOver GameState { getDirection = UP, getPosition= P1 } = True 
checkGameOver _ = False 

update :: Float -> GameState -> GameState
update seconds gameState = 
  if isGameOver gameState
  then gameState 
  else newGameState 
  where
    direction = getDirection gameState
    oldPosition = getPosition gameState 
    
    newGameState = gameState { getPosition = newPosition, isGameOver = newIsGameOver }
    newIsGameOver = checkGameOver gameState 
    newPosition = 
      case direction of 
      UP -> decrease oldPosition
      DOWN -> increase oldPosition

createPositionToken :: GameState -> Picture 
createPositionToken GameState { getPosition = pos }  = 
  color blue 
  $ scale 1 (-1)
  $ translate 0 (toFloat pos * 100 - 300)
  $ rectangleSolid 200 200

createGameInfo :: GameState -> Picture
createGameInfo GameState { getPosition = pos , getDirection = dir } 
  = color black
  $ translate (-300) 200
  $ scale 0.1 0.1
  $ text $ "Position: " <> show pos <> " Direction: "  <> show dir

createGameOverText :: GameState -> Picture
createGameOverText (GameState gameEnded _ _) =
  let 
    playText = [] 
    endText = 
      [ color blue $ translate (-200) 0 $ scale 0.5 0.5 $ text "GAME OVER" 
      , color green $ translate (-200) (-100) $ scale 0.2 0.2 $ text "Press \"SPACE\" to try again!"
      ]
  in pictures $ if gameEnded then endText else playText 
 
-- A function to convert the gameState to a picture.
render :: GameState -> Picture
render gameState = 
  let 
    info = createGameInfo gameState
    text = createGameOverText gameState 
    token = createPositionToken gameState
  in pictures 
      [ info
      , token 
      , text
      ]

main :: IO ()
main = 
  play 
    displayMode 
    backgroundColor 
    numberSteps 
    (createInitialGameState True)
    render 
    handleKeys 
    update 
