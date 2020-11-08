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
  |collisionWithAsteroid (player gstate) (asteroids gstate) == False = return $ gstate {  player = updatePlayer (player gstate),
                                                                                          asteroids = moveAsteroids (asteroids gstate) (bullets gstate),
                                                                                          bullets = moveBullet (bullets gstate),
                                                                                          elapsedTime = elapsedTime gstate + secs}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
   -- if any key other than z show player
    | c == 'p' = gstate {player = NoPlayer}
    | c == 'u' = gstate {player = Player (Collider (0,0) 20) (0,0)} 
    | c == 'b' = gstate {bullets = createBullet (player gstate):bullets gstate} 
    | c == 'm' = gstate {asteroids = spawnAsteroid:spawnAsteroid2:spawnAsteroid3:spawnAsteroid4:asteroids gstate}
    | otherwise = gstate {player = movePlayer(player gstate) c}

inputKey _ gstate = gstate -- Otherwise keep the same


-- enemy functions --

spawnAsteroid :: Asteroid
spawnAsteroid = Asteroid (Collider (-200,-300) 30) (5,10)

spawnAsteroid2 :: Asteroid
spawnAsteroid2 = Asteroid (Collider (200,-300) 30) (-10,20)

spawnAsteroid3 :: Asteroid
spawnAsteroid3 = Asteroid (Collider (-200,300) 30) (10,-5)

spawnAsteroid4 :: Asteroid
spawnAsteroid4 = Asteroid (Collider (200,300) 30) (-20,-10)

moveAsteroids :: [Asteroid] -> [Bullet] -> [Asteroid]
moveAsteroids [] _ = []
moveAsteroids [NoAsteroid] _ = [NoAsteroid]
moveAsteroids (NoAsteroid:xs) bullets = (NoAsteroid:moveAsteroids xs bullets)
moveAsteroids ((Asteroid(Collider(x,y) z) (a,b)):xs) bullets
  | collisionWithBullet (bullets) (Asteroid(Collider(x,y) z) (a,b)) == True = (NoAsteroid:moveAsteroids xs bullets)
  | otherwise = ((Asteroid(Collider ((x+a),(y+b)) z) (a,b)):moveAsteroids xs bullets)


-- collision functions -- 

collisionWithAsteroid :: Player -> [Asteroid] -> Bool
collisionWithAsteroid NoPlayer _ = False
collisionWithAsteroid (Player(Collider (x,y) z) (a,b)) (NoAsteroid:xs) = collisionWithAsteroid (Player(Collider (x,y) z) (a,b)) xs
collisionWithAsteroid (Player(Collider (x,y) z) (a,b)) [] = False
collisionWithAsteroid (Player(Collider (x,y) z) (a,b)) (Asteroid (Collider(x2,y2) z2) (a2,b2):xs)
  | abs(x-x2) < z+z2 && abs(y-y2) < z+z2 = True
  | otherwise = collisionWithAsteroid (Player(Collider (x,y) z) (a,b)) xs

collisionWithBullet :: [Bullet] -> Asteroid -> Bool
collisionWithBullet [] _ = False
collisionWithBullet ((Bullet(Collider ((x),(y)) z) (a,b)):xs) NoAsteroid = False
collisionWithBullet ((Bullet(Collider ((x),(y)) z) (a,b)):xs) (Asteroid (Collider(x2,y2) z2) (a2,b2))
  | abs(x-x2) < z+z2 && abs(y-y2) < z+z2 = True
  | otherwise = collisionWithBullet xs (Asteroid (Collider(x2,y2) z2) (a2,b2))



-- player functions --

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


-- bullet functions -- 

--We are giving bullet, the location of the player . So that in the end it appears that the bullet is shot from Player 
createBullet :: Player -> Bullet
createBullet NoPlayer = NotShooting
createBullet (Player (Collider (x,y) z) (a,b))
  | a == 0 && b == 0 = Bullet (Collider (x,y) 4) (20,0)
  | a > 10 || b > 10 = Bullet (Collider (x,y) 4) (a*2.5,b*2.5)
  | a > 0 || b > 0 = Bullet (Collider (x,y) 4) (a*3.5,b*3.5)
  | a < 0 || b < 0 = Bullet (Collider (x,y) 4) (a*3.5,b*3.5)
  | a < 10 || b < 10 = Bullet (Collider (x,y) 4) (a*2.5,b*2.5)
  | otherwise = Bullet (Collider (x,y) 4) (20,0)

--moving bullet to y=x direction by 10 pixels
moveBullet :: [Bullet] -> [Bullet]
moveBullet [] = []
moveBullet [NotShooting] = [NotShooting]
moveBullet ((Bullet(Collider(x,y) z) (a,b)):xs) = ((Bullet(Collider ((x+a),(y+b)) z) (a,b)):moveBullet xs)

