module Draw where

import Graphics.Gloss
import Types

-- | Function that draws the game menu
drawMenu :: World -> Picture
drawMenu World {actual = Menu op} = pictures [drawTitle, drawPlayButton (op == Play), drawExitButton (op == Exit)]

drawTitle :: Picture
drawTitle = scale 0.5 0.5 $ translate (-400) 300 $ text "Snake Game"

drawPlayButton :: Bool -> Picture
drawPlayButton selected = color black $ translate 0 0 $ drawButton "Play" selected

drawExitButton :: Bool -> Picture
drawExitButton selected = color black $ translate 0 (-50) $ drawButton "Exit" selected

drawButton :: String -> Bool -> Picture
drawButton label selected =
  translate 0 0 $
    pictures
      [ color (chooseColor selected) $ rectangleSolid largura altura,
        color (chooseTextColor selected) $ scale 0.2 0.2 $ translate (-100) 25 $ scale 0.7 0.7 $ text label
      ]
  where
    chooseColor True = black -- Button color while selected
    chooseColor False = white -- Button color while not selected
    chooseTextColor True = white -- Text color while selected
    chooseTextColor False = black -- Text color while not selected
    largura = 120
    altura = 60

-- | Function that draws the game
drawGame :: World -> Picture
drawGame world = pictures $ drawSnake (getSnake world)
  where
    getSnake :: World -> Snake
    getSnake World {snake = (a, b) : t} = (a, b) : t

drawSnake :: Snake -> [Picture]
drawSnake [] = []
drawSnake ((a, b) : t) = (translate a b $ color red $ rectangleSolid 20 20) : drawSnake t

-- | Function that draws the Game Over screen
drawGameOver :: Picture
drawGameOver = pictures [Text "Game Over", Text "Press Space to go back to the main menu"]