module AssetManager where

import Paths_pacman

{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON)

-- Standard Lib imports
import System.IO.Unsafe
import Data.Maybe


-- Custom package imports
import Graphics.Gloss
import Graphics.Gloss.Juicy


-- Handmade imports
import Model


imgDoor = pngByFile "door.png"
wVertical = pngByFile "vertical.png"
wHorizontal = pngByFile "horizontal.png"
imgEmpty = pngByFile "empty.png"
imgCoin = pngByFile "coin.png"
imgDot = pngByFile "dot.png"
pacmanUp = pngByFile "pacmanUp.png"
pacmanDown = pngByFile "pacmanDown.png"
pacmanLeft = pngByFile "pacmanLeft.png"
pacmanRight = pngByFile "pacmanRight.png"

gRed = pngByFile "red.png"
gPink = pngByFile "pink.png"
gCyan = pngByFile "cyan.png"
gOrange = pngByFile "orange.png"
gFrightened = pngByFile "frightened.png"

getDataFN :: FilePath -> IO FilePath
getDataFN name = do
  dir <- getDataDir
  return (dir ++ "\\app\\assets\\" ++ name)

getFilePath::String -> FilePath
getFilePath s = unsafePerformIO $ getDataFN s

pngByFile::FilePath -> Picture
pngByFile p = dontDoThis $ loadJuicyPNG $ getFilePath p

-- Justification for this function: If the assets don't load, the game isn't worth much.
dontDoThis :: IO (Maybe Picture) -> Picture
dontDoThis = fromJust . unsafePerformIO
