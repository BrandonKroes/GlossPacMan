module Constants where

-- * The constants module is used to prevent random magical numbers in the code base.

-- * Frames per second for the game.
fps::Int
fps = 10

-- * The amount of time a ghost is frightened before returning to the state.
frightTime::Float
frightTime = 10


boardWidth :: Int
boardWidth = 28

boardHeight :: Int
boardHeight = 31

screenWidth :: Int
screenWidth = 1000

screenHeight :: Int
screenHeight = 1000

originHeight :: Float
originHeight = fromIntegral (-235)

originWidth :: Float
originWidth = fromIntegral (240)

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral boardWidth

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral boardHeight

getTimeOutTime::Int->Int
getTimeOutTime 1 = 100000000000
getTimeOutTime sequenceId = [7, 20, 7, 20, 5, 20, 5, maxBound-100000] !! sequenceId
