module Colisions where

import System.Random
import Types

-- Top left corner, bottom right corner
hitbox :: Coordinates -> Hitbox
hitbox (x, y) = ((x - 10, y + 10), (x + 10, y - 10))

colision :: Hitbox -> Hitbox -> Bool
colision ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = x1 < x4 && x2 > x3 && y1 > y4 && y2 < y3

appleColision :: Coordinates -> Snake -> Bool
appleColision (ax, ay) [] = False
appleColision (ax, ay) (x : xs) = colision appleHitbox headHitbox
  where
    appleHitbox = hitbox (ax, ay)
    headHitbox = hitbox x

-- TODO:
{-
create a hitbox function
create a function to check if the head of the snake collides with the apple (use hitbox)
create a function to check if the head of the snake collides with the body (use hitbox). in that case, make snake = initialSnake and change actual to GameOver
random apple generation?
change the snake head color?
update README.md
-}