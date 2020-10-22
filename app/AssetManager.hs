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

pacmanUp2 = pngByFile "pacmanUp2.png"
pacmanDown2 = pngByFile "pacmanDown2.png"
pacmanLeft2 = pngByFile "pacmanLeft2.png"
pacmanRight2 = pngByFile "pacmanRight2.png"

pacmanUp = pngByFile "pacmanUp.png"
pacmanDown = pngByFile "pacmanDown.png"
pacmanLeft = pngByFile "pacmanLeft.png"
pacmanRight = pngByFile "pacmanRight.png"

gRed = pngByFile "red.png"
gPink = pngByFile "pink.png"
gCyan = pngByFile "cyan.png"
gOrange = pngByFile "orange.png"
gFrightened = pngByFile "frightened.png"
gRetreat = pngByFile "retreat.png"


gRed2 = pngByFile "red2.png"
gPink2 = pngByFile "pink2.png"
gCyan2 = pngByFile "cyan2.png"
gOrange2 = pngByFile "orange2.png"
gFrightened2 = pngByFile "frightened2.png"




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
