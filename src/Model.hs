-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

data Player = NoPlayer
              |Player {collider :: Collider, velocity :: Velocity} 
              deriving (Eq,Show)

data Bullet = NotShooting 
            |B Collider
            deriving (Eq,Show)

data Collider = Collider {position :: Point, radius :: Float} 
  deriving (Eq,Show)

data Asteroid = NoAsteroid
                |Asteroid Collider Velocity   
    deriving (Eq,Show)

data Health = NoHealth
              |H Int
type Point = (Float, Float)
type Velocity = (Float, Float)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                  player :: Player
                , asteroids :: Asteroid
                , bullets :: Bullet
                , health :: Health
                , elapsedTime :: Float
                 }



initialState :: GameState
initialState = GameState NoPlayer NoAsteroid NotShooting NoHealth 0


