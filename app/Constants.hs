module Constants where




--multiplyToSeconds::Float->Float

fps::Int
fps = 15

toSeconds::Float->Float
toSeconds x = (0.0005 * fromIntegral fps) * x

frightTime::Float
frightTime = 10
