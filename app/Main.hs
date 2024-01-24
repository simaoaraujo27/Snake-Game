module Main where

import Draw
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Types
import GHC.Base (undefined)

-- | Game Display
gameDisplay :: Display
gameDisplay = InWindow "Snake Game" (600, 600) (0, 0)

-- | Background Color
backgroundColor :: Color
backgroundColor = white

-- | Frame-rate
frameRate :: Int
frameRate = 60

-- | Initial world
initialWorld :: World
initialWorld =
  World
    { snake = [(0, 0), (20, 0), (40, 0)],
      food = (30, 30),
      direction = East,
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
handleInput (EventKey (SpecialKey KeyDown) Down _ _) world@(World {actual = Menu op}) = nextOption world
handleInput (EventKey (SpecialKey KeyUp) Down _ _) world@(World {actual = Menu op}) = nextOption world
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) world@(World {actual = Menu Play}) = world {actual = Playing}
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) world@(World {actual = Menu Exit}) = error "Game Closed"
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world@(World {actual = GameOver}) = initialWorld
handleInput _ w = w


nextOption :: World -> World
nextOption world@(World {actual = Menu Play}) = world {actual = Menu Exit}
nextOption world@(World {actual = Menu Exit}) = world {actual = Menu Play}

-- | A function to update the World
updateWorld :: Float -> World -> World
updateWorld = undefined


main :: IO ()
main =
  play
    gameDisplay
    backgroundColor
    frameRate
    initialWorld
    drawWorld
    handleInput
    (\t w -> w)
