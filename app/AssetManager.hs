module AssetManager where

import Paths_pacman


import GHC.Generics

-- Standard Lib imports
import Data.Maybe


-- Custom package imports
import Graphics.Gloss
import Graphics.Gloss.Juicy


-- * Function used to get the textures from drive.
-- * The getDataDir is supplied in the Paths_pacman during building.
getDataFN :: FilePath -> IO FilePath
getDataFN name = do
  dir <- getDataDir
  return (dir ++ "\\app\\assets\\" ++ name)


-- * Function that will return a PNG to use for Gloss.
pngByFile::FilePath -> IO Picture
pngByFile p = do
                fp <- getDataFN p
                lj <- loadJuicyPNG fp
                return (fromJust lj)
