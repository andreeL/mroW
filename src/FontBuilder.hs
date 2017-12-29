{-# LANGUAGE RecordWildCards #-}
module FontBuilder
  ( buildFont
  ) where

import FreeTypeHelpers (FTGlyph(..),  getGlyphs)
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad
import Control.Monad.ST
import Data.Word
import Foreign.C.Types
import Data.Char (intToDigit)

buildFont :: Int -> IO()
buildFont fontSize = do
  glyphs <- getGlyphs fontSize "abcdefg"
  forM_ glyphs $ \(character, FTGlyph{..}) -> do
    let (width, height) = _size
    let noOfElements = width * height
    --let (finalVector, _, _) = buildGlyph myVector (bitmapWidth, bitmapRows)
    let printInfo var value = putStrLn $ var ++ " = " ++ (show value)
    printInfo "character" character
    printInfo "size" _size
    printInfo "bearing" _bearing
    printInfo "advance" _advance
    forM_ [0..height - 1] $ \y -> do
      forM_ [0..width - 1] $ \x -> do
        let index = fromIntegral $ x + y * width
        let intensity = (fromIntegral $ _image ! index) * 15 / 255
        let character = if intensity == 0 then ' ' else intToDigit (ceiling  intensity)
        putChar $ character
        putChar $ character
      putStrLn ""