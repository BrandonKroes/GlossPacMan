module Rendering where

-- Gloss imports
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Custom packages
import AssetManager
import Model
import Render.Override
import Render.Player
import Render.World
import Render.Stats

render :: GameState -> IO Picture
render g = do 
            rp <- renderPlayers g
            rw <- renderWorld g
            rs <- renderStats g 
            return (pictures $ renderOverride g $ rs ++ rw ++ rp)
