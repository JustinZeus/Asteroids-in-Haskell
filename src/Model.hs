-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

<<<<<<< Updated upstream
=======
data Player = NoPlayer
                |Player Collider -- Health Bullet 
data Bullet = B Collider
data Collider = Collider {position :: Point, radius :: Float} 
data Health = H Int
type Point = (Float, Float)

>>>>>>> Stashed changes
nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
<<<<<<< Updated upstream
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
=======
                player :: Player
                , elapsedTime :: Float
>>>>>>> Stashed changes
                 }


initialState :: GameState
initialState = GameState NoPlayer 0