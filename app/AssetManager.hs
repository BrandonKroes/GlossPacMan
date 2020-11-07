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


imgDoor = ppngByFile "door.png"
wVertical = ppngByFile "vertical.png"
wHorizontal = ppngByFile "horizontal.png"
imgEmpty = ppngByFile "empty.png"
imgCoin = ppngByFile "coin.png"
imgDot = ppngByFile "dot.png"

pacmanUp2 = ppngByFile "pacmanUp2.png"
pacmanDown2 = ppngByFile "pacmanDown2.png"
pacmanLeft2 = ppngByFile "pacmanLeft2.png"
pacmanRight2 = ppngByFile "pacmanRight2.png"

pacmanUp = ppngByFile "pacmanUp.png"
pacmanDown = ppngByFile "pacmanDown.png"
pacmanLeft = ppngByFile "pacmanLeft.png"
pacmanRight = ppngByFile "pacmanRight.png"

gRed = ppngByFile "red.png"
gPink = ppngByFile "pink.png"
gCyan = ppngByFile "cyan.png"
gOrange = ppngByFile "orange.png"
gFrightened = ppngByFile "frightened.png"
gRetreat = ppngByFile "retreat.png"


gRed2 = ppngByFile "red2.png"
gPink2 = ppngByFile "pink2.png"
gCyan2 = ppngByFile "cyan2.png"
gOrange2 = ppngByFile "orange2.png"
gFrightened2 = ppngByFile "frightened2.png"




getDataFN :: FilePath -> IO FilePath
getDataFN name = do
  dir <- getDataDir
  return (dir ++ "\\app\\assets\\" ++ name)



pngByFile::FilePath -> IO Picture
pngByFile p = do
                fp <- getDataFN p
                lj <- loadJuicyPNG fp
                return (fromJust lj)


getFilePaths::String -> FilePath
getFilePaths s = unsafePerformIO $ getDataFN s


ppngByFile::FilePath -> Picture
ppngByFile p = dontDoThis $ loadJuicyPNG $ getFilePaths p

-- Justification for this function: If the assets don't load, the game isn't worth much.
dontDoThis :: IO (Maybe Picture) -> Picture
dontDoThis = fromJust . unsafePerformIO
