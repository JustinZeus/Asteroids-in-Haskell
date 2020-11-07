-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

spaceship :: Picture
spaceship = color white (polygon [(0,0),(-50,50),(-50,50),(0,0)])

position_sps :: Player -> Picture
position_sps (Player (C(x,y) f) _ _) = translate x y spaceship

viewPure :: GameState -> Picture
