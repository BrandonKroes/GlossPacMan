module Rendering where

-- Gloss imports
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Custom packages
import Model
import Render.Override
import Render.Player
import Render.World
import Render.Stats

render :: GameState -> IO Picture
render g = return (pictures $ renderOverride g $ renderStats g ++ renderWorld g ++ renderPlayers g)
