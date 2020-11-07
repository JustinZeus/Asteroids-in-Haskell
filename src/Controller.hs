-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate { elapsedTime = elapsedTime gstate + secs}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
   -- if any key other than z show player
    | c == 'p' = gstate {player = NoPlayer}
    | c == 'u' = gstate { player = Player (Collider (0,0) 20) (0,0)}
inputKey _ gstate = gstate -- Otherwise keep the same  | ch == 'o' = Player(Collider (x,(y+25)) z) v       --up
  | otherwise = Player(Collider (x,y) z) v
movePlayer NoPlayer ch = NoPlayer
