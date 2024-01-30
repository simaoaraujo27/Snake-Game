module Main where

import Colisions
import Draw
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Types

-- | Game Display
gameDisplay :: Display
gameDisplay = InWindow "Snake Game" (600, 600) (0, 0)

-- | Background Color
backgroundColor :: Color
backgroundColor = white

-- | Frame-rate
frameRate :: Int
frameRate = 8

-- | Initial world
initialWorld :: World
initialWorld =
  World
    { snake = [(0, 0)],
      food = (20, 20),
      direction = West,
      actual = Menu Play
    }

-- | A function to convert the world a picture
drawWorld :: World -> Picture
drawWorld world@World {actual = Menu op} = drawMenu world
drawWorld world@World {snake = s, food = (fx, fy), direction = dir, actual = Playing} = drawGame world
drawWorld world@World {actual = GameOver} = drawGameOver
drawWorld _ = Blank

-- | A function to handle input events
handleInput :: Event -> World -> World
-- Menu and GameOver
handleInput (EventKey (SpecialKey KeyDown) Down _ _) world@(World {actual = Menu op}) = nextOption world
handleInput (EventKey (SpecialKey KeyUp) Down _ _) world@(World {actual = Menu op}) = nextOption world
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) world@(World {actual = Menu Play}) = world {actual = Playing}
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) world@(World {actual = Menu Exit}) = error "Game Closed"
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world@(World {actual = GameOver}) = initialWorld {actual = Playing}
-- Playing
handleInput (EventKey (SpecialKey KeyRight) Down _ _) world@(World {snake = s, food = a, direction = dir, actual = Playing}) =
  if dir == West then world else world {direction = East}
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) world@(World {snake = s, food = a, direction = dir, actual = Playing}) =
  if dir == East then world else world {direction = West}
handleInput (EventKey (SpecialKey KeyUp) Down _ _) world@(World {snake = s, food = a, direction = dir, actual = Playing}) =
  if dir == South then world else world {direction = North}
handleInput (EventKey (SpecialKey KeyDown) Down _ _) world@(World {snake = s, food = a, direction = dir, actual = Playing}) =
  if dir == North then world else world {direction = South}
handleInput _ w = w

nextOption :: World -> World
nextOption world@(World {actual = Menu Play}) = world {actual = Menu Exit}
nextOption world@(World {actual = Menu Exit}) = world {actual = Menu Play}

-- | A function to update the World
updateWorld :: Float -> World -> World
updateWorld dt world@World {snake = s, food = a, direction = dir, actual = Playing}
  | snakeColision s = world {actual = GameOver}
  | appleColision a s = world {snake = move dir (s ++ [((\(x, y) -> (x + 20, y)) (last s))]), food = (100, 100)}
  | otherwise = world {snake = move dir s}
updateWorld dt w = w

-- | Moves the snake
move :: Direction -> Snake -> Snake
move dir [] = []
move dir snake@(head : tail) =
  case dir of
    North -> if snd head >= 300 then moveTo snake (fst head, -300) else moveTo snake (fst head, snd head + 20)
    South -> if snd head <= (-300) then moveTo snake (fst head, 300) else moveTo snake (fst head, snd head - 20)
    West -> if fst head <= (-300) then moveTo snake (300, snd head) else moveTo snake (fst head - 20, snd head)
    East -> if fst head >= 300 then moveTo snake (-300, snd head) else moveTo snake (fst head + 20, snd head)
  where
    -- \| Saves the current position in order to move the whole snake
    moveTo :: Snake -> Coordinates -> Snake
    moveTo [] _ = []
    moveTo (p : ps) destination = destination : moveTo ps p

main :: IO ()
main =
  play
    gameDisplay
    backgroundColor
    frameRate
    initialWorld
    drawWorld
    handleInput
    updateWorld
