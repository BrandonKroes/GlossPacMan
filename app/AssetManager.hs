module AssetManager where

import Paths_pacman

{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics

-- Standard Lib imports
import Data.Maybe


-- Custom package imports
import Graphics.Gloss
import Graphics.Gloss.Juicy


getDataFN :: FilePath -> IO FilePath
getDataFN name = do
  dir <- getDataDir
  return (dir ++ "\\app\\assets\\" ++ name)


pngByFile::FilePath -> IO Picture
pngByFile p = do
                fp <- getDataFN p
                lj <- loadJuicyPNG fp
                return (fromJust lj)
