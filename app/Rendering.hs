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

render :: GameState -> Picture
render g = pictures $ renderOverride g $ renderStats g ++ renderWorld g ++ renderPlayers g
