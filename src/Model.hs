-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

type Point = (Float,Float)
data Collider = C Point Float 
data Player = Player Collider Health Bullet 
data Health = H Int
data Bullet = B Collider

initialState :: GameState
initialState = GameState ShowNothing 0