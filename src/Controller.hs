-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  |collisionWithAsteroid (player gstate) (asteroids gstate) == True = return $ gstate {player = NoPlayer, elapsedTime = elapsedTime gstate + secs}
  |collisionWithAsteroid (player gstate) (asteroids gstate) == False = return $ gstate {player = updatePlayer (player gstate), elapsedTime = elapsedTime gstate + secs}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
   -- if any key other than z show player
    | c == 'p' = gstate {player = NoPlayer}
    | c == 'u' = gstate {player = Player (Collider (0,0) 20) (0,0)} 
    | c == 'm' = gstate {asteroids = spawnAsteroid:spawnAsteroid2:[]}
    | otherwise = gstate {player = movePlayer(player gstate) c}

inputKey _ gstate = gstate -- Otherwise keep the same

spawnAsteroid :: Asteroid
spawnAsteroid = Asteroid (Collider (-200,-300) 30) (0,0)

spawnAsteroid2 :: Asteroid
spawnAsteroid2 = Asteroid (Collider (200,-300) 30) (0,0)

collisionWithAsteroid :: Player -> [Asteroid] -> Bool
collisionWithAsteroid NoPlayer _ = False
collisionWithAsteroid (Player(Collider (x,y) z) (a,b)) [] = False
collisionWithAsteroid (Player(Collider (x,y) z) (a,b)) (Asteroid (Collider(x2,y2) z2) (a2,b2):xs)
  | abs(x-x2) < z+z2 && abs(y-y2) < z+z2 = True
  | otherwise = collisionWithAsteroid (Player(Collider (x,y) z) (a,b)) xs



movePlayer :: Player -> Char -> Player
movePlayer (Player(Collider (x,y) z) (a,b)) ch 
  | ch == 'd' =  Player(Collider (x,y) z) (a+5,b)       --right
  | ch == 'a' = Player(Collider (x,y) z) (a-5,b)        --left
  | ch == 's' = Player(Collider (x,y) z) (a,b-5)        --down
  | ch == 'w' = Player(Collider (x,y) z) (a,b+5)        --up
  | otherwise = Player(Collider (x,y) z) (a,b) 
movePlayer NoPlayer ch = NoPlayer


updatePlayer :: Player -> Player
updatePlayer player = 
  case player of
    NoPlayer -> NoPlayer
    (Player(Collider (x,y) z) (a,b)) -> (Player(Collider (x+a,y+b) z) (a,b))

--We are giving bullet, the location of the player . So that in the end it appears that the bullet is shot from Player 
createBullet :: Player -> Bullet
createBullet (Player (Collider (x,y) z) _) = Bullet (Collider (x,y) z)

--moving bullet to y=x direction by 10 pixels
moveBullet :: Bullet -> Bullet
moveBullet NotShooting = NotShooting
moveBullet (Bullet (Collider (x,y) z)) = Bullet(Collider ((x+10),(y+10)) z)