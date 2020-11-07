-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

data Player = NoPlayer
              |Player {collider :: Collider, velocity :: Velocity} 
              deriving (Eq,Show)
data Bullet = B Collider
  deriving (Eq,Show)
data Collider = Collider {position :: Point, radius :: Float} 
  deriving (Eq,Show)
data Health = NoHealth
              |H Int
type Point = (Float, Float)
type Velocity = (Float, Float)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                  player :: Player
                , health :: Health
                , elapsedTime :: Float
                 }


initialState :: GameState
initialState = GameState NoPlayer NoHealth 0

updatePlayerPos :: Player -> Maybe (Float, Float)
updatePlayerPos player@(Player c v)
    | player == NoPlayer = Nothing
    | otherwise = Just (useVelocity c v)

useVelocity :: (Float, Float) -> Float -> (Float, Float) -> (Float, Float)
useVelocity _ z (x, y) = (0,0)
useVelocity (x,y) z (a,b) = (x+a, y+b)
