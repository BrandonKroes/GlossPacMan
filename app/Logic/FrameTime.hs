module Logic.FrameTime where

import Model

updateFrameTime::GameState -> Float -> GameState
updateFrameTime gstate deltaTime= updateAnimationInterval $  updateAnimationTime deltaTime $ updateGameTime gstate deltaTime

updateGameTime::GameState -> Float -> GameState
updateGameTime gstate deltaTime= gstate {time = deltaTime}


flipVal::Int -> Int
flipVal 1 = 2
flipVal 2 = 1
flipVal _ = 1


updateAnimationTime::Float -> GameState -> GameState
updateAnimationTime deltaTime gstate = gstate {animationTime = deltaTime + animationTime gstate}

resetAnimationTime::GameState->GameState
resetAnimationTime gstate = gstate {animationTime=0.0}

updateAnimationInterval:: GameState -> GameState
updateAnimationInterval gstate | 0.35 <= animationTime gstate = resetAnimationTime gstate {animationInterval = flipVal $ animationInterval gstate } | otherwise = gstate
