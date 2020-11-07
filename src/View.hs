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
bullet = color white (Circle(20))

position_sps :: Player -> Picture
position_sps (Player (C(x,y) _) _ _) = translate x y spaceship

position_bul :: Bullet -> Picture
position_bul (B (C (x,y) _)) = translate x y bullet -- 


viewPure :: GameState -> Picture
viewPure gstate = case player gstate of
        NoPlayer   -> scale 0.2 0.2 (translate (-1800) 1400 (color red (text "press u to unpause the game")))
        Player (Collider (x,y) z) v -> pictures [translate x y (color white (circle(z))), scale 0.2 0.2 (translate (-1800) 1400 (color red (text "press p to pause the game")))]
