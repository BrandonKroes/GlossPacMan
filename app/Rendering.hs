module Rendering where


import Render.Override
import Render.Player
import Render.World


import AssetManager
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model

render :: GameState -> Picture
render g = pictures $ renderOverride g  $
                      renderWorld g ++
                      renderPlayers g
