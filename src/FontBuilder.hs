{-# LANGUAGE RecordWildCards #-}
module FontBuilder
  ( Font
  , Glyph(..)
  , createGUIFont
  , createFont
  , showFont -- only for debuging
  ) where

import Control.Exception (tryJust)
import Control.Monad (forM_, foldM, guard)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.Char (intToDigit)
import Data.Either (either)
import Data.Foldable (foldl')
import Data.Bifunctor (bimap)
import qualified Data.Map as Map (Map, fromList)
import Data.Vector.Unboxed as VConst (Vector, (!), fromList, zipWith, unsafeFreeze, unsafeThaw, generate)
import Data.Vector.Unboxed.Mutable as VMutable (MVector, read, write, replicate)
import Data.Word (Word8)
import qualified FreeTypeHelpers as FTH (FTGlyph(..), createGlyphs)
import Text.Read (readMaybe)
import System.IO.Error (isDoesNotExistError)

type Vec2 = (Int, Int)

data Glyph = Glyph {
  -- seems like we loose some precision on bearing and advance, maybe we should use floats instead
  _bearing :: Vec2,
  _advance :: Int,
  _imagePos :: Vec2
} deriving (Show, Read)

type Font = Map.Map Char Glyph

type Image a = (Vec2, a)
type FontAndTexture = (Font, Image (VConst.Vector Word8))
maxDelta = 30000 :: Int

getSquareLength :: Vec2 -> Int
getSquareLength (dX, dY) = dX * dX + dY * dY

getLength :: Vec2 -> Float
getLength = sqrt . fromIntegral . getSquareLength

-- In a lot of these functions the values passed are simply assumed to be correct
-- maybe I'll make it more robust in the future, and add some extra boundary checks or something
-- However since no unsafe read/writes are made we should be pretty ok!

createGUIFont :: IO FontAndTexture
createGUIFont = do
  let fontCacheFilepath = "guiFont.tmp"
  cachedFont <- readFromFile fontCacheFilepath
  case cachedFont of
    Just font -> putStrLn "Using cached font!" *> pure font
    Nothing -> do
      let ascii = ['\0'..'\127'] -- we only care for the basic character set for now
      putStrLn "Generating font, this might take several minutes... (do NOT do this in GHCi, it can crash)"
      -- TODO: WARNING, GHCi crashes when trying to build fonts this highres... this is probably a good place to start optimizing :P
      font <- createFont Nothing 26 3 16 ascii
      writeToFile fontCacheFilepath font
      pure font

readFromFile :: Read a => String -> IO (Maybe a)
readFromFile filepath = either (const Nothing) readMaybe <$> tryJust (guard . isDoesNotExistError) (readFile filepath)

writeToFile :: Show a => String -> a -> IO ()
writeToFile filepath = writeFile filepath . show

createFont :: Maybe String -> Int -> Int -> Int -> [Char] -> IO FontAndTexture
createFont maybeFontPath glyphHeight borderSize scaleDown chars = do
  ftGlyphs <- FTH.createGlyphs maybeFontPath (Nothing, (glyphHeight * scaleDown)) chars
  pure $ buildFontFromFtGlyphs glyphHeight borderSize scaleDown ftGlyphs

showFont :: FontAndTexture -> IO ()
showFont (font, ((textureWidth, textureHeight), textureData)) = do
  forM_ (reverse [0..textureHeight - 1]) $ \y -> do
    forM_ [0..textureWidth - 1] $ \x -> do
      let index = fromIntegral $ x + y * textureWidth
          intensity = (fromIntegral $ textureData ! index) * 15 / 255
          character = if intensity == 0 then ' ' else intToDigit (ceiling  intensity)
      putChar $ character
      putChar $ character
    putStrLn ""

  putStrLn $ show font

buildFontFromFtGlyphs :: Int -> Int -> Int -> [(Char, FTH.FTGlyph)] -> FontAndTexture
buildFontFromFtGlyphs requestedGlyphHeight borderSize scaleDown ftGlyphs = do
  let glyphSize@(glyphWidth, glyphHeight) = (requestedGlyphHeight + 2 * borderSize, requestedGlyphHeight + 2 * borderSize) -- currently we only support square glyph sizes
      iSquareRoot = ceiling . sqrt . fromIntegral
      iLog2 = ceiling . logBase 2 . fromIntegral
      textureHeight = (2^) . iLog2 . (*glyphHeight) . iSquareRoot . length $ ftGlyphs
      textureSize@(textureWidth, _) = (textureHeight, textureHeight) -- currently we only support square textures with the size beeing a power of two
      glyphData = buildGlyphData glyphSize borderSize scaleDown textureSize ftGlyphs
      font = Map.fromList . fmap (\(char, glyph, _) -> (char, glyph)) $ glyphData
      textureData = buildTextureData textureSize glyphData
   in (font, (textureSize, textureData))

buildTextureData :: Vec2 -> [(Char, Glyph, Image (VConst.Vector Float))] -> VConst.Vector Word8
buildTextureData (textureWidth, textureHeight) glyphData = runST $ do
  textureData <- (VMutable.replicate (textureWidth * textureHeight) 255)
  forM_ glyphData $ \(_, glyph, ((glyphWidth, glyphHeight), glyphSDF)) -> do
    let getGlyphPixel x y = normalize $ glyphSDF ! (x + y * glyphWidth)
        (xPos, yPos) = _imagePos glyph
    forM_ [0..glyphHeight - 1] $ \y -> do
      forM_ [0..glyphWidth - 1] $ \x -> do
        VMutable.write textureData (xPos + x + (yPos + y) * textureWidth) (getGlyphPixel x y)
  unsafeFreeze textureData
  where normalize = fromIntegral . (+128) . max (-128) . min (127) . round

buildGlyphData :: Vec2 -> Int -> Int -> Vec2 -> [(Char, FTH.FTGlyph)] -> [(Char, Glyph, Image (VConst.Vector Float))]
buildGlyphData glyphSize@(glyphWidth, glyphHeight) borderSize scaleDown (textureWidth, textureHeight) ftGlyphs =
  let gridPositions = [(x, y) | y <- [0, glyphHeight..textureHeight - glyphHeight], x <- [0, glyphWidth..textureWidth - glyphHeight]]
      buildGlyphTuple (imagePos, (char, ftGlyph@FTH.FTGlyph{..})) =
        let norm = (`div` scaleDown)
            (ftWidth, ftHeight) = _size
            (ftBearingX, ftBearingY) = _bearing
            glyph = Glyph {
              _bearing = ((norm ftBearingX) - borderSize, (norm (ftBearingY - ftHeight)) - borderSize),
              _advance = norm _advance,
              _imagePos = imagePos
            }
        in (char, glyph, buildGlyphSDF borderSize scaleDown ftGlyph)
    -- TODO: this is easily parallelized if to slow
    in fmap buildGlyphTuple $ zip gridPositions ftGlyphs

buildGlyphSDF :: Int -> Int -> FTH.FTGlyph -> Image (VConst.Vector Float)
buildGlyphSDF borderSize scaleDown FTH.FTGlyph{..} =
  let scaledBorderSize = borderSize * scaleDown
      apply f = bimap f f
      targetSize = apply (+(2 * scaledBorderSize)) _size
      offset = (scaledBorderSize, scaledBorderSize)
      glyphImage = (_size, _image)
      fullSizeSDF = buildSDF targetSize offset glyphImage
  in scaledDownSDF fullSizeSDF scaleDown

scaledDownSDF :: Image (VConst.Vector Float) -> Int -> Image (VConst.Vector Float)
scaledDownSDF ((sourceWidth, sourceHeight), sourceData) scaleDown =
  let targetSize@(targetWith, _) = (sourceWidth `div` scaleDown, sourceHeight `div` scaleDown)
      getAverage i =
        let x = i `mod` targetWith
            y = i `div` targetWith
            js = [(x * scaleDown + u + (y * scaleDown + v) * sourceWidth) | u <- [0..scaleDown-1], v <- [0..scaleDown-1]]
            total = foldr (\j acc -> (sourceData ! j) + acc) 0 js
            average = total / (fromIntegral $ scaleDown * scaleDown)
        in average

  in (targetSize, VConst.generate (uncurry (*) targetSize) getAverage)

buildSDF :: Vec2 -> Vec2 -> Image (Vector Word8) -> Image (VConst.Vector Float)
buildSDF size offset source =
  let posDF = buildDF size offset (0, maxDelta) source
      negDF = buildDF size offset (maxDelta, 0) source
    in (size, VConst.zipWith (\a b -> (getLength a) - (getLength b)) posDF negDF)

buildDF :: Vec2 -> Vec2 -> (Int, Int) -> Image (Vector Word8) -> VConst.Vector Vec2
buildDF size offset values source = runST $ do
  df <- VConst.unsafeThaw $ createBinaryMask size offset values source
  run8SSEDT (size, df) *> unsafeFreeze df

createBinaryMask :: Vec2 -> Vec2 -> (Int, Int) -> Image (Vector Word8) -> VConst.Vector Vec2
createBinaryMask (width, height) (offsetX, offsetY) (zeroValue, oneValue) ((sourceWidth, sourceHeight), sourceData) =
  let threshold = 128
      getValueAt x y =
        let x' = x - offsetX
            y' = y - offsetY
            value = if (
                      x' >= 0 && x' < sourceWidth
                      && y' >= 0 && y' < sourceHeight
                      && sourceData ! (x' + (sourceHeight - 1 - y') * sourceWidth) >= threshold
                    )
                    then zeroValue
                    else oneValue
        in (value, value)
  in VConst.fromList [getValueAt x y | y <- [0..height - 1], x <- [0..width - 1]]

run8SSEDT :: PrimMonad m => Image (VMutable.MVector (PrimState m) Vec2) -> m ()
run8SSEDT ((width, height), df) = do
  let safeGetPoint x y =
        if (x >= 0 && y >= 0 && x < width && y < height)
          then VMutable.read df (x + y * width)
          else pure (maxDelta, maxDelta)

  let pickBest x y offsets = do
        let index = x + y * width
        thisPoint <- VMutable.read df index
        (best, _) <- foldM (\currentBest (offsetx, offsety) -> do
          other <- bimap (+offsetx) (+offsety) <$> safeGetPoint (x + offsetx) (y + offsety)
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
      xForward = [0..width - 1]
      xBackward = reverse xForward
      yForward = [0..height - 1]
      yBackward = reverse yForward

  forM_ yForward $ \y -> do
    forM_ xForward $ \x -> pickBest x y mask1
    forM_ xBackward $ \x -> pickBest x y mask2
  forM_ yBackward $ \y -> do
    forM_ xBackward $ \x -> pickBest x y mask3
    forM_ xForward $ \x -> pickBest x y mask4
