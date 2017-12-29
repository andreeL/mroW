{-# LANGUAGE RecordWildCards #-}
module FreeTypeHelpers
  ( FTGlyph(..)
  , getGlyphs
  , showGlyphInfos -- only for debuging
  ) where

import Control.Monad (forM, forM_, when)
import Data.Vector.Unboxed (Vector, generateM)
import Data.Char (intToDigit)
import Data.Word (Word8)
import Foreign (alloca, peek, peekElemOff)
import Foreign.C.String (castCCharToChar, withCString)

-- FreeType imports
import Graphics.Rendering.FreeType.Internal
import qualified Graphics.Rendering.FreeType.Internal.Bitmap as FTB
--import qualified Graphics.Rendering.FreeType.Internal.BitmapGlyph as FTBG
import qualified Graphics.Rendering.FreeType.Internal.GlyphMetrics as FTGM
import Graphics.Rendering.FreeType.Internal.Face
--import qualified Graphics.Rendering.FreeType.Internal.Glyph as FTG
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.GlyphSlot
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
--import Graphics.Rendering.FreeType.Internal.Vector

data FTGlyph = FTGlyph {
  _size :: (Int, Int),
  _bearing :: (Int, Int),
  _advance :: Int,
  _image :: Vector Word8
}

showFailmessage :: String -> FT_Error -> IO ()
showFailmessage operation error = putStrLn $ operation ++ " failed: " ++ show error

runFtOperation :: IO FT_Error -> IO FT_Error
runFtOperation action = do
  error <- action
  when (error /= 0) $ fail $ "runFtOperation failed with error - " ++ show error
  pure error

withFreeType :: (FT_Library -> IO a) -> (FT_Error -> IO a) -> IO a
withFreeType f fail = alloca $ \ftLibPtr -> do
  result <- ft_Init_FreeType ftLibPtr
  if (result == 0)
    then do
      ftLib <- peek ftLibPtr
      res <- f ftLib
      ft_Done_FreeType ftLib
      pure res
    else fail result

withNewFace :: FT_Library -> String -> FT_Long
  -> (FT_Face -> IO a)
  -> (FT_Error -> IO a)
  -> IO a
withNewFace ftLib fontName faceIndex f fail =
  alloca $ \ftFacePtr -> do
    withCString fontName $ \fontNameCString -> do
      result <- ft_New_Face ftLib fontNameCString faceIndex ftFacePtr
      if (result == 0)
        then do
          ftFace <- peek ftFacePtr
          res <- f ftFace
          ft_Done_Face ftFace
          pure res
        else fail result

withGlyphSlot :: (FT_GlyphSlot -> IO a) -> FT_Face -> Char -> IO a
withGlyphSlot f ftFace character = do
  runFtOperation $ ft_Load_Char ftFace (fromIntegral . fromEnum $ character) ft_LOAD_RENDER
  ftGlyphSlot <- peek $ glyph ftFace
  f ftGlyphSlot

extractFTGlyph ::  FT_GlyphSlot -> IO FTGlyph
extractFTGlyph ftGlyphSlot = do
  ftMetrics <- peek $ metrics ftGlyphSlot
  ftBitmap <- peek $ bitmap ftGlyphSlot
  let buffer = FTB.buffer ftBitmap
  let norm = (`div` 64) . fromIntegral
  let _size = (fromIntegral $ FTB.width ftBitmap, fromIntegral $ FTB.rows ftBitmap)
  let _bearing = (norm . FTGM.horiBearingX $ ftMetrics, norm . FTGM.horiBearingY $ ftMetrics)
  let _advance = norm . FTGM.horiAdvance $ ftMetrics
  _image <- generateM (uncurry (*) _size) (fmap fromIntegral . peekElemOff buffer)
  pure FTGlyph{..}

getGlyphs :: Int -> [Char] -> IO [(Char, FTGlyph)]
getGlyphs fontSize chars = do
  flip withFreeType (fmap (const []) . showFailmessage "init freetype")  $ \ftLib -> do
    flip (withNewFace ftLib "fonts/GibFontPlox.otf" 0) (fmap (const []) . showFailmessage "load font") $ \ftFace -> do
      runFtOperation $ ft_Set_Pixel_Sizes ftFace 0 (fromIntegral fontSize)
      forM chars $ \char -> withGlyphSlot (fmap ((,) char) . extractFTGlyph) ftFace char

-- debug stuff
showGlyphInfo :: (Char, FT_GlyphSlot) -> IO()
showGlyphInfo (character, ftGlyphSlot) = do
  ftMetrics <- peek $ metrics ftGlyphSlot
  let printInfo var value = putStrLn $ var ++ " = " ++ (show value)
  let normalize = (/64) . fromIntegral
  printInfo "code" character
  --printInfo "metrics: width" $ normalize (FTGM.width ftMetrics)
  --printInfo "metrics: height" $ normalize (FTGM.height ftMetrics)
  printInfo "metrics: horiBearingX" $ normalize (FTGM.horiBearingX ftMetrics)
  printInfo "metrics: horiBearingY" $ normalize (FTGM.horiBearingY ftMetrics)
  printInfo "metrics: horiAdvance" $ normalize (FTGM.horiAdvance ftMetrics)
  printInfo "metrics: vertBearingX" $ normalize (FTGM.vertBearingX ftMetrics)
  printInfo "metrics: vertBearingY" $ normalize (FTGM.vertBearingY ftMetrics)
  printInfo "metrics: vertAdvance" $ normalize (FTGM.vertAdvance ftMetrics)
  ftBitmap <- peek $ bitmap ftGlyphSlot

  let bitmapWidth = FTB.width ftBitmap
  let bitmapRows = FTB.rows ftBitmap
  let bitmapBufferPtr = FTB.buffer ftBitmap
  printInfo "bitmap: width" $ bitmapWidth
  printInfo "bitmap: rows" $ bitmapRows
  --printInfo "bitmap: num_grays" $ FTB.num_grays ftBitmap
  --printInfo "bitmap: pixel_mode" $ FTB.pixel_mode ftBitmap
  --printInfo "bitmap: palette_mode" $ FTB.palette_mode ftBitmap
  if (FTB.pitch ftBitmap /= bitmapWidth)
    then do
      printInfo "bitmap: pitch" $ FTB.pitch ftBitmap
      putStrLn "pitch is DIFFERENT from width!"
    else do
      forM_ [0..bitmapRows - 1] $ \y -> do
        forM_ [0..bitmapWidth - 1] $ \x -> do
          let index = fromIntegral $ x + y * bitmapWidth
          cChar <- peekElemOff bitmapBufferPtr index
          --putStr $ (castCCharToChar cChar)
          let intensity = (fromIntegral . fromEnum . castCCharToChar $ cChar) * 15 / 255
          let character = if intensity == 0 then ' ' else intToDigit (floor intensity)
          putChar $ character
          putChar $ character
        putStrLn ""

  -- alloca $ \ftGlyphPtr -> do
  --   runFtOperation $ ft_Get_Glyph ftGlyphSlot ftGlyphPtr
  --   ftGlyph <- peek ftGlyphPtr
  --   glyphAdvance <- peek $ FTG.advance ftGlyph
  --   glyphSlotAdvance <- peek $ advance ftGlyphSlot
  --   printInfo "glyphSlot: advanceX" ((/64) . fromIntegral . x $ glyphSlotAdvance)
  --   printInfo "glyphSlot: advanceY" ((/64) . fromIntegral . y $ glyphSlotAdvance)
  --   printInfo "glyph: advance" glyphAdvance
  --   let ftBitmapGlyph = FTBG.cast ftGlyph
  --   glyphLeft <- peek $ FTBG.left ftBitmapGlyph
  --   printInfo "glyph: left" glyphLeft
  --   glyphTop <- peek $ FTBG.top ftBitmapGlyph
  --   printInfo "glyph: top" glyphTop

showGlyphInfos :: Int -> [Char] -> IO()
showGlyphInfos charSize chars =
  flip withFreeType (showFailmessage "init freetype") $ \ftLib -> do
    flip (withNewFace ftLib "fonts/GibFontPlox.otf" 0) (showFailmessage "load font") $ \ftFace -> do
      runFtOperation $ ft_Set_Pixel_Sizes ftFace 0 (fromIntegral charSize)
      forM_ chars $ \char -> withGlyphSlot (showGlyphInfo . ((,) char)) ftFace char
