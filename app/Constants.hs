module Constants where




--multiplyToSeconds::Float->Float

fps::Int
fps = 5

toSeconds::Float->Float
toSeconds x = (0.0005 * fromIntegral fps) * x

frightTime::Float
frightTime = 10