-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
-- Multiple phases indicate what should happen every iteration --
step :: Float -> GameState -> IO GameState
step secs gstate
  | (gamePhase gstate) == Start = return $ gstate {player = NoPlayer, asteroids = spawnAsteroid, elapsedTime = 0}
  | (gamePhase gstate) == Pause = return $ gstate {gamePhase = Pause}
  | (gamePhase gstate) == Dead = return $ gstate {player = NoPlayer, elapsedTime = 0}
  | (gamePhase gstate) == Playing && collisionWithAsteroid (player gstate) (asteroids gstate) == True = return $ gstate {gamePhase = Dead, player = NoPlayer, elapsedTime = 0}
  | (gamePhase gstate) == Playing && collisionWithAsteroid (player gstate) (asteroids gstate) == False && (elapsedTime gstate) > 3 = do
  randomNumber <- randomRIO (-400,400)  :: IO Float
  randomNumber2 <- randomRIO (-400,400) :: IO Float    
  randomNumber5 <- randomRIO (-400,400) :: IO Float
  randomNumber3 <- randomRIO (-20,20)   :: IO Float
  randomNumber4 <- randomRIO (-20,20)   :: IO Float
  let chosennumber = randomNumber
  let chosennumber2 = randomNumber2
  let chosennumber3 = randomNumber3
  let chosennumber4 = randomNumber4
  return $ gstate {player = updatePlayer (player gstate),score = (score gstate) + checkAsteroids (asteroids gstate) (bullets gstate),asteroids = (Asteroid(Collider (chosennumber,chosennumber2) 30) (chosennumber3,chosennumber4)):(Asteroid(Collider (150,chosennumber) 30) (chosennumber3,chosennumber4)):(moveAsteroids (asteroids gstate) (bullets gstate)),bullets = moveBullet (bullets gstate),elapsedTime = 0}
  | (gamePhase gstate) == Playing && collisionWithAsteroid (player gstate) (asteroids gstate) == False = return $ gstate {  player = updatePlayer (player gstate),
                                                                                                                            score = (score gstate) + checkAsteroids (asteroids gstate) (bullets gstate),
                                                                                                                            asteroids = moveAsteroids (asteroids gstate) (bullets gstate),
                                                                                                                            bullets = moveBullet (bullets gstate),
                                                                                                                            elapsedTime = elapsedTime gstate + secs}

-- | Handle user input
-- different keystrokes influence phase changes, spawning of bullets and player movement
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)
;
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
   -- if any key other than z show player
    | c == 'o' && (gamePhase gstate) == Pause = gstate {gamePhase = Playing}
    | c == 'p' && (gamePhase gstate) == Playing = gstate {gamePhase = Pause}
    | c == 'r' && (gamePhase gstate) == Dead = gstate {gamePhase = Playing, player = Player (Collider (0,0) 20) (0,0), bullets = [], asteroids = spawnAsteroid, score = 0}
    | c == 'r' && (gamePhase gstate) == Playing = gstate {gamePhase = Playing, player = Player (Collider (0,0) 20) (0,0), bullets = [], asteroids = [], score = 0}
    | c == 'o' && (gamePhase gstate) == Start = gstate {gamePhase = Playing, player = Player (Collider (0,0) 20) (0,0)} 
    | c == 'b' && (gamePhase gstate) == Playing = gstate {bullets = createBullet (player gstate):bullets gstate} 
    | otherwise = gstate {player = movePlayer(player gstate) c gstate}

inputKey _ gstate = gstate -- Otherwise keep the same


-- enemy functions --

spawnAsteroid :: [Asteroid]
spawnAsteroid = [Asteroid (Collider (-200,-300) 30) (5,10),Asteroid (Collider (200,-300) 30) (-10,20),Asteroid (Collider (-200,300) 30) (10,-5), Asteroid (Collider (200,300) 30) (-20,-10)]

--goes through the list of asteroids and bullets and if they collide makes the asteroids dissapear

moveAsteroids :: [Asteroid] -> [Bullet] -> [Asteroid]
moveAsteroids [] _ = []
moveAsteroids [NoAsteroid] _ = [NoAsteroid]
moveAsteroids (NoAsteroid:xs) bullets = (NoAsteroid:moveAsteroids xs bullets)
moveAsteroids ((Asteroid(Collider(x,y) z) (a,b)):xs) bullets
  | collisionWithBullet (bullets) (Asteroid(Collider(x,y) z) (a,b)) == True = (NoAsteroid:moveAsteroids xs bullets)
  | otherwise = ((Asteroid(Collider ((x+a),(y+b)) z) (a,b)):moveAsteroids xs bullets)

--goes through the list of asteroids and bullets and if they collide in the next function makes the score go up

checkAsteroids :: [Asteroid] -> [Bullet] -> Int
checkAsteroids [] _ = 0
checkAsteroids [NoAsteroid] _ = 0
checkAsteroids (NoAsteroid:xs) bullets = 0 + (checkAsteroids xs bullets)
checkAsteroids ((Asteroid(Collider(x,y) z) (a,b)):xs) bullets
  | collisionWithBullet (bullets) (Asteroid(Collider(x,y) z) (a,b)) == True = 100 + (checkAsteroids xs bullets)
  | otherwise = 0 + (checkAsteroids xs bullets)



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
collisionWithBullet _ NoAsteroid = False
collisionWithBullet ((Bullet(Collider ((x),(y)) z) (a,b)):xs) (Asteroid (Collider(x2,y2) z2) (a2,b2))
  | abs(x-x2) < z+z2 && abs(y-y2) < z+z2 = True
  | otherwise = collisionWithBullet xs (Asteroid (Collider(x2,y2) z2) (a2,b2))

-- player functions --

movePlayer :: Player -> Char -> GameState -> Player
movePlayer (Player(Collider (x,y) z) (a,b)) ch gstate
  | ch == 'd' && a < 10  && (gamePhase gstate) == Playing = Player(Collider (x,y) z) (a+3,b)        --right
  | ch == 'a' && a > -10 && (gamePhase gstate) == Playing = Player(Collider (x,y) z) (a-3,b)        --left
  | ch == 'w' && b < 10 && (gamePhase gstate) == Playing = Player(Collider (x,y) z) (a,b+3)         --up
  | ch == 's' && b > -10 && (gamePhase gstate) == Playing = Player(Collider (x,y) z) (a,b-3)        --down
  | otherwise = Player(Collider (x,y) z) (a,b) 
movePlayer NoPlayer ch gstate = NoPlayer

updatePlayer :: Player -> Player
updatePlayer NoPlayer = NoPlayer
updatePlayer (Player(Collider (x,y) z) (a,b))
                                                | (check (x,y) (-400,400)) = Player(Collider (x+a,y+b) z) (a,b)
                                                | otherwise                =  Player(Collider (0,0) z) (a,b)

 
check :: (Float,Float) -> (Float,Float) -> Bool
check (x,y) (min,max) = x >= min && x <= max && y >= min && y <= max 


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


-- save top score in a file
scoreSave :: GameState -> IO ()
scoreSave gstate = writeFile "score.txt" (show (score gstate))