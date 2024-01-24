module Types where

data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

data Option
  = Play
  | Exit
  deriving (Eq)

data Actual
  = Menu Option
  | Playing
  | GameOver

type Coordinates = (Float, Float)

type Snake = [Coordinates]

data World = World
  { snake :: Snake,
    food :: Coordinates,
    direction :: Direction,
    actual :: Actual
  }