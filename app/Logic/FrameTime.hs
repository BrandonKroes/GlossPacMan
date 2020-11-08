module Logic.FrameTime where

import Model


-- * The main function of the module.
-- * The module manages the internal clock of the game for animation time and game time (the time the game has been running)
updateFrameTime::GameState -> Float -> GameState
updateFrameTime gstate deltaTime= updateAnimationInterval $  updateAnimationTime deltaTime $ updateGameTime gstate deltaTime


-- * Incrementing the amount of time the game has been running with the frame time.
updateGameTime::GameState -> Float -> GameState
updateGameTime gstate deltaTime= gstate {time = deltaTime+ (time gstate)}


-- * The animationInterval only has two states. Whenever the flip is called it will be changed.
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
