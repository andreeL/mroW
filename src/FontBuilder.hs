{-# LANGUAGE RecordWildCards #-}

module FontBuilder
  ( buildFont
  , showFont -- only for debuging
  ) where

import Control.Arrow ((***))
import Control.Monad (forM_, foldM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.Vector.Unboxed as VConst (Vector, (!), fromList, zipWith, unsafeFreeze, unsafeThaw)
import Data.Vector.Unboxed.Mutable as VMutable (MVector, read, write, unsafeNew)
import Data.Char (intToDigit)
import Data.Word (Word8)
import qualified Data.Map as Map (Map, fromList)
import qualified FreeTypeHelpers as FTH (FTGlyph(..), createGlyphs)

type Vec2 = (Int, Int)

data Glyph = Glyph {
  _bearing :: Vec2,
  _advance :: Int,
  _imagePos :: Vec2
} deriving (Show)

data Font = Font {
  _size :: Vec2,
  _glyphs :: Map.Map Char Glyph
} deriving (Show)

maxDelta = 10000 :: Int

getSquareLength :: Vec2 -> Int
getSquareLength (dX, dY) = dX * dX + dY * dY

getLength :: Vec2 -> Float
getLength = sqrt . fromIntegral . getSquareLength

buildFont :: Maybe String -> Int -> Int -> [Char] -> IO (Font, (Vec2, VConst.Vector Word8))
buildFont maybeFontPath glyphHeight borderSize chars = do
  ftGlyphs <- FTH.createGlyphs maybeFontPath (Nothing, glyphHeight - borderSize) chars
  pure $ buildFontFromFtGlyphs glyphHeight borderSize ftGlyphs

showFont :: (Font, (Vec2, VConst.Vector Word8)) -> IO ()
showFont (font, ((textureWidth, textureHeight), textureData)) = do
  forM_ [0..textureHeight - 1] $ \y -> do
    forM_ [0..textureWidth - 1] $ \x -> do
      let index = fromIntegral $ x + y * textureWidth
          intensity = (fromIntegral $ textureData ! index) * 15 / 255
          character = if intensity == 0 then ' ' else intToDigit (ceiling  intensity)
      putChar $ character
      putChar $ character
    putStrLn ""

  putStrLn $ show font
  
buildFontFromFtGlyphs :: Int -> Int -> [(Char, FTH.FTGlyph)] -> (Font, (Vec2, VConst.Vector Word8))
buildFontFromFtGlyphs glyphHeight borderSize ftGlyphs = do
  let fontSize@(glyphWidth, _) = (glyphHeight, glyphHeight) -- currently we only support square glyph sizes
      integerSquareRoot = ceiling . sqrt . fromIntegral
      integerLog2 = ceiling . logBase 2 . fromIntegral
      textureHeight = (2^) . integerLog2 . (*glyphHeight) . integerSquareRoot . length $ ftGlyphs
      textureSize@(textureWidth, _) = (textureHeight, textureHeight) -- currently we only support square textures with the size beeing a power of two
      glyphData = buildGlyphData fontSize textureSize ftGlyphs
      font = Font fontSize . Map.fromList . fmap (\(char, glyph, _) -> (char, glyph)) $ glyphData
      textureData = buildTextureData fontSize textureSize glyphData
   in (font, (textureSize, textureData))

buildTextureData :: Vec2 -> Vec2 -> [(Char, Glyph, VConst.Vector Word8)] -> VConst.Vector Word8
buildTextureData (glyphWidth, glyphHeight) (textureWidth, textureHeight) glyphData = runST $ do
  textureData <- (VMutable.unsafeNew (textureWidth * textureHeight))
  forM_ glyphData $ \(_, glyph, glyphSDF) -> do
    let getGlyphPixel x y = glyphSDF ! (x + y * glyphWidth)
        (xPos, yPos) = _imagePos glyph
    forM_ [0..glyphHeight - 1] $ \y -> do
      forM_ [0..glyphWidth - 1] $ \x -> do
        VMutable.write textureData (xPos + x + (yPos + y) * textureWidth) (getGlyphPixel x y)
  unsafeFreeze textureData

buildGlyphData :: Vec2 -> Vec2 -> [(Char, FTH.FTGlyph)] -> [(Char, Glyph, VConst.Vector Word8)]
buildGlyphData glyphSize@(glyphWidth, glyphHeight) (textureWidth, textureHeight) ftGlyphs =
  let gridPositions = [(x, y) | y <- [0, glyphHeight..textureHeight - 1], x <- [0, glyphWidth..textureWidth - 1]]
      buildGlyphTuple (imagePos, (char, ftGlyph)) =
        let glyph = Glyph {
          _bearing = FTH._bearing ftGlyph,
          _advance = FTH._advance ftGlyph,
          _imagePos = imagePos
        } in (char, glyph, buildSDF glyphSize ftGlyph)
    in fmap buildGlyphTuple $ zip gridPositions ftGlyphs -- TODO: this is easily parallelized if to slow

buildSDF :: Vec2 -> FTH.FTGlyph -> VConst.Vector Word8
buildSDF glyphSize ftGlyph =
  let posDF = buildDF glyphSize 0 maxDelta ftGlyph
      negDF = buildDF glyphSize maxDelta 0 ftGlyph
    in VConst.zipWith (\a b ->
      let lengthA = getLength $ a
          distance = if lengthA == 0 then 1 - (getLength b) else lengthA
        in (fromIntegral . (+128) . max (-128) . min (127) . round . (* 8) $ distance)
    ) posDF negDF

buildDF :: Vec2 -> Int -> Int -> FTH.FTGlyph -> VConst.Vector Vec2
buildDF glyphSize zeroVal oneVal ftGlyph = runST $ do
    df <- VConst.unsafeThaw $ createBinaryMask glyphSize zeroVal oneVal ftGlyph
    run8SSEDT glyphSize df *> unsafeFreeze df
    
createBinaryMask :: Vec2 -> Int -> Int -> FTH.FTGlyph -> VConst.Vector Vec2
createBinaryMask (glyphWidth, glyphHeight) zeroVal oneVal FTH.FTGlyph{..} =
  let (sourceWidth, sourceHeight) = _size
      (offsetX, offsetY) = ((glyphWidth - sourceWidth) `div` 2, (glyphHeight - sourceHeight) `div` 2)
      getVal x y = getGlyphPixel (x - offsetX) (y - offsetY)
      getGlyphPixel x y =
        let val = if (x >= 0 && x < sourceWidth && y >= 0 && y < sourceHeight)
                    then if _image ! (x + y * sourceWidth) < 128 then oneVal else zeroVal
                    else oneVal
         in (val, val)

  in VConst.fromList [getVal x y | y <- [0..glyphHeight - 1], x <- [0..glyphWidth - 1]]

run8SSEDT :: PrimMonad m => Vec2 -> VMutable.MVector (PrimState m) Vec2 -> m ()
run8SSEDT (glyphWidth, glyphHeight) df = do
  let safeGetPoint x y =
        if (x >= 0 && y >= 0 && x < glyphWidth && y < glyphHeight)
          then VMutable.read df (x + y * glyphWidth)
          else pure (maxDelta, maxDelta)

  let pickBest x y offsets = do
        let index = x + y * glyphWidth
        thisPoint <- VMutable.read df index
        (best, _) <- foldM (\currentBest (offsetx, offsety) -> do
          other <- fmap ((+offsetx) *** (+offsety)) $ safeGetPoint (x + offsetx) (y + offsety)
          let otherDistance = getSquareLength other
          pure $ if (otherDistance < snd currentBest)
            then (other, otherDistance)
            else currentBest
          ) (thisPoint, getSquareLength thisPoint) offsets
        VMutable.write df index best

  let mask1 = [(-1, -1), (0, -1), (1, -1), (-1, 0)] :: [Vec2]
      mask2 = [(1, 0)] :: [Vec2]
      mask3 = [(-1, 1), (0, 1), (1, 1), (1, 0)] :: [Vec2]
      mask4 = [(-1, 0)] :: [Vec2]
      xForward = [0..glyphWidth - 1]
      xBackward = reverse xForward
      yForward = [0..glyphHeight - 1]
      yBackward = reverse yForward

  forM_ yForward $ \y -> do
    forM_ xForward $ \x -> pickBest x y mask1
    forM_ xBackward $ \x -> pickBest x y mask2
  forM_ yBackward $ \y -> do
    forM_ xBackward $ \x -> pickBest x y mask3
    forM_ xForward $ \x -> pickBest x y mask4
