-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

data Player = Player Collider Health Bullet 
data Bullet = B Collider
data Collider = C Point Float 
data Health = H Int
type Point = (Float,Float)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   player :: Player
                 , bullets :: [Bullet]
                 , colliders :: [Collider] 
                 , elapsedTime :: Float
                 , infoToShow  :: InfoToShow
                 , health :: Health
                 }



initialState :: GameState
initialState = GameState ShowNothing 0