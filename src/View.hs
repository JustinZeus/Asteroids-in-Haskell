-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

-- gets the combined pictures that are generated every step --
viewPure :: GameState -> Picture
viewPure gstate = pictures [playerimg, pausemsg, asteroid, bulletimg, scorevw]
        where
                playerimg = viewPlayer gstate
                pausemsg = viewPause gstate
                asteroid = viewAsteroids gstate
                bulletimg = viewBullets gstate
                scorevw = viewScore gstate
       


-- depending on the gamestate player information either draw a playable circle in the right location or dont do anything
viewPlayer :: GameState -> Picture
viewPlayer gstate = 
         case player gstate of
                NoPlayer   -> Blank
                Player (Collider (x,y) z) v -> translate x y (color white (circleSolid (z)))


-- depending on the gamephase draw a bit of text to show controlls
viewPause :: GameState -> Picture
viewPause gstate = 
         case gamePhase gstate of
                Start   -> scale 0.2 0.2 (translate (-1800) 1400 (color white (text "press o to start the game")))
                Dead -> scale 0.2 0.2 (translate (-1800) 1400 (color white (text "press r to restart the game and save your highscore")))
                Pause -> scale 0.2 0.2 (translate (-1800) 1400 (color white (text "press o to unpause the game")))
                Playing -> scale 0.2 0.2 (translate (-1800) 1400 (color white (text "press p to pause the game or r to reset")))


-- loop through all the asteroids in the gameState and draw them according to their positions --
viewAsteroids :: GameState -> Picture
viewAsteroids gstate = pictures [translate x y (color red (circle(z))) | Asteroid (Collider (x,y) z) v <- asteroids gstate]

-- loop through all the bullets in the gameState and draw them according to their positions --

viewBullets :: GameState -> Picture
viewBullets gstate = pictures [translate x y (color blue (circle(z))) | (Bullet (Collider (x,y) z) v)<- bullets gstate]


-- Displays current score --
viewScore :: GameState -> Picture
viewScore gstate = pictures [scale 0.2 0.2 (translate (-1800) 1700 (color white (text "Score"))), scale 0.2 0.2 (translate (-1400) 1700 (color white (text (show (score gstate)))))]