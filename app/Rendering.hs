module Rendering where


import Render.Override
import Render.Player
import Render.World
import Render.Stats


import AssetManager
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model

render :: GameState -> Picture
render g = pictures $ renderOverride g  $
                      renderStats g ++
                      renderWorld g ++
                      renderPlayers g
