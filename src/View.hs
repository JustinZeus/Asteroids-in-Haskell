-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

spaceship :: Picture
spaceship = color white (polygon [(0,0),(-50,50),(-50,50),(0,0)])

bullet :: Picture
bullet = color white (Circle(5)) 

--the position of the player is needed so that we could move it
position_sps :: Player -> Picture
position_sps (Player (Collider (x,y) z) v) = translate x y spaceship


viewPure :: GameState -> Picture
viewPure gstate = pictures [playerimg, pausemsg, asteroid, bulletimg, scorevw]
        where
                playerimg = viewPlayer gstate
                pausemsg = viewPause gstate
                asteroid = viewAsteroids gstate
                bulletimg = viewBullets gstate
                scorevw = viewScore gstate
       

viewPlayer :: GameState -> Picture
viewPlayer gstate = 
         case player gstate of
                NoPlayer   -> Blank
                Player (Collider (x,y) z) v -> translate x y (color white (circleSolid (z)))

viewPause :: GameState -> Picture
viewPause gstate = 
         case gamePhase gstate of
                Start   -> scale 0.2 0.2 (translate (-1800) 1400 (color white (text "press o to start the game")))
                Dead -> scale 0.2 0.2 (translate (-1800) 1400 (color white (text "press r to restart the game")))
                Pause -> scale 0.2 0.2 (translate (-1800) 1400 (color white (text "press o to unpause the game")))
                Playing -> scale 0.2 0.2 (translate (-1800) 1400 (color white (text "press p to pause the game or r to reset")))

viewAsteroids :: GameState -> Picture
viewAsteroids gstate = pictures [translate x y (color red (circle(z))) | Asteroid (Collider (x,y) z) v <- asteroids gstate]

viewBullets :: GameState -> Picture
viewBullets gstate = pictures [translate x y (color blue (circle(z))) | (Bullet (Collider (x,y) z) v)<- bullets gstate]

viewScore :: GameState -> Picture
viewScore gstate = pictures [scale 0.2 0.2 (translate (-1800) 1700 (color white (text "Score"))), scale 0.2 0.2 (translate (-1400) 1700 (color white (text (show (score gstate)))))]