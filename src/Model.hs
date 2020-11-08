-- | This module contains the data types
--   which represent the state of the game
module Model where

 -- GamePhase data type --
data GamePhase = Playing
                | Dead
                | Start
                | Pause
                deriving (Eq,Show)

 -- Player and bullet data types -- 
data Player = NoPlayer
              |Player {collider :: Collider, velocity :: Velocity} 
              deriving (Eq,Show)

data Bullet = NotShooting 
            |Bullet Collider Velocity
            deriving (Eq,Show)

 -- Collider and health data types --
data Collider = Collider {position :: Point, radius :: Float} 
  deriving (Eq,Show)

data Asteroid = NoAsteroid
                |Asteroid Collider Velocity   
    deriving (Eq,Show)

data Health = NoHealth
              |H Int


  -- Point and velocity definitions --
type Point = (Float, Float)
type Velocity = (Float, Float)


nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

 -- GameState data type -- 
data GameState = GameState {
                  gamePhase :: GamePhase
                , player :: Player
                , asteroids :: [Asteroid]
                , bullets :: [Bullet]
                , health :: Health
                , elapsedTime :: Float
                , score :: Int
                 }


 -- initial GameState --
initialState :: GameState
initialState = GameState Start NoPlayer [] [] NoHealth 0 0


