{-# LANGUAGE RecordWildCards #-}

module FontBuilder
  ( buildFont
  ) where

import FreeTypeHelpers (FTGlyph(..), getGlyphs)
import Data.Vector.Unboxed as VConst (Vector, (!), fromList, zipWith, unsafeFreeze, unsafeThaw)
import Data.Vector.Unboxed.Mutable as VMutable (MVector, read, write)
import Control.Monad (forM_, foldM)
import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Word (Word8)
import Data.Char (intToDigit)
import Control.Arrow ((***))

getSquareLength :: (Int, Int) -> Int
getSquareLength (dX, dY) = dX * dX + dY * dY

getLength :: (Int, Int) -> Float
getLength = sqrt . fromIntegral . getSquareLength

extraBorder = 2 :: Int -- TODO: maybe pass as parameter
maxDelta = 10000 :: Int

createBinaryMask :: FTGlyph -> (Int, Int) -> Int -> Int -> VConst.Vector (Int, Int)
createBinaryMask FTGlyph{..} (newWidth, newHeight) zeroVal oneVal =
  let (width, height) = _size
      getVal x y = getGlyphPixel x y -- TODONOW! add some offset depending on _bearing
      getGlyphPixel x y =
        let val = if (x >= 0 && x < width && y >= 0 && y < height)
                    then if _image ! (x + y * width) < 128 then oneVal else zeroVal
                    else oneVal
         in (val, val)

  in VConst.fromList [getVal x y | y <- [0..newHeight - 1], x <- [0..newWidth - 1]]
  
run8SSEDT :: PrimMonad m => (Int, Int) -> VMutable.MVector (PrimState m) (Int, Int) -> m ()
run8SSEDT (newWidth, newHeight) df = do
  let safeGetPoint x y =
        if (x >= 0 && y >= 0 && x < newWidth && y < newHeight)
          then VMutable.read df (x + y * newWidth)
          else pure (maxDelta, maxDelta)
    
  let pickBest x y offsets = do
        let index = x + y * newWidth
        thisPoint <- VMutable.read df index
        (best, _) <- foldM (\currentBest (offsetx, offsety) -> do
          other <- fmap ((+offsetx) *** (+offsety)) $ safeGetPoint (x + offsetx) (y + offsety)
          let otherDistance = getSquareLength other
          pure $ if (otherDistance < snd currentBest)
            then (other, otherDistance)
            else currentBest
          ) (thisPoint, getSquareLength thisPoint) offsets
        VMutable.write df index best

  let mask1 = [(-1, 0), (0, -1), (-1, -1), (1, -1)] :: [(Int, Int)]
      mask2 = [(1, 0)] :: [(Int, Int)]
      mask3 = [(1, 0), (0, 1), (-1, 1), (1, 1)] :: [(Int, Int)]
      mask4 = [(-1, 0)] :: [(Int, Int)]
      xForward = [0..newWidth - 1]
      xBackward = reverse xForward
      yForward = [0..newHeight - 1]
      yBackward = reverse yForward
      
  forM_ yForward $ \y -> do
    forM_ xForward $ \x -> pickBest x y mask1
    forM_ xBackward $ \x -> pickBest x y mask2
  forM_ yBackward $ \y -> do
    forM_ xBackward $ \x -> pickBest x y mask3
    forM_ xForward $ \x -> pickBest x y mask4

buildDF :: FTGlyph -> (Int, Int) -> Int -> Int -> VConst.Vector (Int, Int)
buildDF ftGlyph newSize zeroVal oneVal = runST $ do
    df <- VConst.unsafeThaw $ createBinaryMask ftGlyph newSize zeroVal oneVal
    run8SSEDT newSize df *> unsafeFreeze df

buildSDF :: FTGlyph -> (Int, Int) -> VConst.Vector Word8
buildSDF ftGlyph@FTGlyph{..} newSize@(newWidth, newHeight) =
  let posDF = buildDF ftGlyph newSize 0 maxDelta
      negDF = buildDF ftGlyph newSize maxDelta 0
   in VConst.zipWith (\a b ->
      let distance = (getLength a) - (getLength b)
       in (fromIntegral . (+128) . max (-128) . min (127) . round . (* 10) $ distance)
    ) posDF negDF

buildFont :: Int -> IO ()
buildFont fontSize = do
  glyphs <- getGlyphs fontSize "Testing some characters..."
  forM_ glyphs $ \(character, glyph@FTGlyph{..}) -> do
    let (width, height) = _size
    let noOfElements = width * height
    --let (finalVector, _, _) = buildGlyph myVector (bitmapWidth, bitmapRows)
    let printInfo var value = putStrLn $ var ++ " = " ++ (show value)
    printInfo "character" character
    printInfo "size" _size
    printInfo "bearing" _bearing
    printInfo "advance" _advance
    let (newWidth, newHeight) = (fontSize + 2 * extraBorder, fontSize + 2 * extraBorder)
    let sdf = buildSDF glyph (newWidth, newHeight)
    forM_ [0..newHeight - 1] $ \y -> do
      forM_ [0..newWidth - 1] $ \x -> do
        let index = fromIntegral $ x + y * newWidth
        let intensity = (fromIntegral $ sdf ! index) * 15 / 255
        let character = if intensity == 0 then ' ' else intToDigit (ceiling  intensity)
        putChar $ character
        putChar $ character
      putStrLn ""
