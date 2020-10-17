module AssetManager where


-- Standard Lib imports
import System.IO.Unsafe
import Data.Maybe


-- Custom package imports
import Graphics.Gloss
import Graphics.Gloss.Juicy

-- Static function calls for images.
-- I'm not sure when and how they are loaded in (i.e. IO calls)
-- TODO: Figure out IO procedure
-- TODO: Making path relative
-- TODO: https://hackage.haskell.org/package/reactive-banana-threepenny-0.7.1.3/src/src/Paths.hs

path = "D:\\VERA\\HU\\Jaar 4\\Minor\\FP\\Game\\GlossPacMan\\app\\assets\\"

door = pngByFile "door.png"
wVertical = pngByFile "vertical.png"
wHorizontal = pngByFile "horizontal.png"
empty = pngByFile "empty.png"
coin = pngByFile "coin.png"
dot = pngByFile "dot.png"
pacmanUp = pngByFile "pacmanUp.png"
pacmanDown = pngByFile "pacmanDown.png"
pacmanLeft = pngByFile "pacmanLeft.png"
pacmanRight = pngByFile "pacmanRight.png"

gRed = pngByFile "red.png"
gPink = pngByFile "pink.png"
gCyan = pngByFile "cyan.png"
gOrange = pngByFile "orange.png"
gFrightened = pngByFile "frightened.png"




jpgByPath::FilePath -> Picture
jpgByPath p = dontDoThis $ loadJuicyJPG (path ++ p)

pngByFile::FilePath -> Picture
pngByFile p = dontDoThis $ loadJuicyPNG (path ++ p)

-- TODO: Find alternative for this anarchy of a function
dontDoThis :: IO (Maybe Picture) -> Picture
dontDoThis = fromJust . unsafePerformIO
