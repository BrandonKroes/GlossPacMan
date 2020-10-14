module Logic where

import Data.Foldable ( asum )

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game




update::Float->GameState->GameState
update secs gstate =  gstate




-- Updating the player
modifyPlayerPosition::(Int, Int)->Player->Player
modifyPlayerPosition (x,y) (PacMan position score direction) = PacMan (x,y) score direction

calculatePlayerPosition::Player->(Int, Int)
calculatePlayerPosition (PacMan (x, y) score direction) | direction==UP = (x-1, y) | direction==DOWN = (x+1, y) | direction==LEFT = (x, y-1) | direction==RIGHT = (x, y+1)

updatePlayer::GameState->GameState
updatePlayer g = g {player= modifyPlayerPosition (calculatePlayerPosition (player g) ) (player g) }


-- Updating the ghost
updateGhost::Player->Player
updateGhost ghost = ghost



--animationInterval∷Float->GameState
-- will update how the animations should function
-- for the player types they will check which way to look at

updateDirection::Player->Direction->Player
updateDirection (PacMan position score direction) d = PacMan position score d

setPlayerDirection::Direction ->GameState->GameState
setPlayerDirection d game = game { player = updateDirection (player game) d}


--togglePauseMode::GameState
transformGameState :: Event -> GameState -> GameState
transformGameState event g = case event of
              EventKey (Char 'w') Down _ _ -> setPlayerDirection  UP g
              EventKey (Char 's') Down _ _ -> setPlayerDirection  DOWN g
              EventKey (Char 'a') Down _ _ -> setPlayerDirection  LEFT g
              EventKey (Char 'd') Down _ _ -> setPlayerDirection  RIGHT g
              EventKey (SpecialKey KeySpace) _ _ _ -> g {pause = not $ pause g}
              _ -> g
