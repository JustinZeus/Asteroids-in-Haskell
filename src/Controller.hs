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
    | otherwise = gstate {player = movePlayer(player gstate) c}

inputKey _ gstate = gstate -- Otherwise keep the same

    


movePlayer :: Player -> Char -> Player
movePlayer (Player(Collider (x,y) z) v) ch 
  | ch == 'r' =  Player(Collider ((x+25),y) z) v      --right
  | ch == 'l' = Player(Collider ((x-25),y) z) v       --left
  | ch == 'd' = Player(Collider (x,(y-25)) z) v       --down
  | ch == 'o' = Player(Collider (x,(y+25)) z) v       --up
  | otherwise = Player(Collider (x,y) z) v
movePlayer NoPlayer ch = NoPlayer

--We are giving bullet, the location of the player . So that in the end it appears that the bullet is shot from Player 
createBullet :: Player -> Bullet
createBullet (Player (Collider (x,y) z) _) = Bullet (Collider (x,y) z)

--moving bullet to y=x direction by 10 pixels
moveBullet :: Bullet -> Bullet
moveBullet NotShooting = NotShooting
moveBullet (Bullet (Collider (x,y) z)) = Bullet(Collider ((x+10),(y+10)) z)