-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case player gstate of
        NoPlayer   -> scale 0.2 0.2 (translate (-1800) 1400 (color red (text "press u to unpause the game")))
        Player (Collider (x,y) z) v -> pictures [translate x y (color white (circle(z))), scale 0.2 0.2 (translate (-1800) 1400 (color red (text "press p to pause the game")))]
