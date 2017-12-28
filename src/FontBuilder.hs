module FontBuilder
  ( showGlyphInfo
  , showGlyphInfos
  ) where

import Control.Monad
import Data.Char
import Foreign
import Foreign.C.String
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

showFailmessage :: String -> FT_Error -> IO ()
showFailmessage operation error = putStrLn $ operation ++ " failed: " ++ show error

runFtOperation :: IO FT_Error -> IO FT_Error
runFtOperation action = do
  error <- action
  when (error /= 0) $ showFailmessage "something" error
  return error

withFreeType :: (FT_Library -> IO a) -> (FT_Error -> IO a) -> IO a
withFreeType f fail = alloca $ \ftLibPtr -> do
  result <- ft_Init_FreeType ftLibPtr
  if (result == 0)
    then do
      ftLib <- peek ftLibPtr
      res <- f ftLib
      ft_Done_FreeType ftLib
      return res
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
          return res
        else fail result

showGlyphInfo :: FT_Face -> Char -> IO()
showGlyphInfo ftFace character = do
  runFtOperation $ ft_Set_Pixel_Sizes ftFace 0 64
  runFtOperation $ ft_Load_Char ftFace (fromIntegral . fromEnum $ character) ft_LOAD_RENDER
  ftGlyphSlot <- peek $ glyph ftFace
  alloca $ \ftGlyphPtr -> do
    runFtOperation $ ft_Get_Glyph ftGlyphSlot ftGlyphPtr
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

    --ftGlyph <- peek ftGlyphPtr
    --glyphAdvance <- peek $ FTG.advance ftGlyph
    --glyphSlotAdvance <- peek $ advance ftGlyphSlot
    --printInfo "glyphSlot: advanceX" ((/64) . fromIntegral . x $ glyphSlotAdvance)
    --printInfo "glyphSlot: advanceY" ((/64) . fromIntegral . y $ glyphSlotAdvance)
    --printInfo "glyph: advance" glyphAdvance
    --let ftBitmapGlyph = FTBG.cast ftGlyph
    --glyphLeft <- peek $ FTBG.left ftBitmapGlyph
    --printInfo "glyph: left" glyphLeft
    --glyphTop <- peek $ FTBG.top ftBitmapGlyph
    --printInfo "glyph: top" glyphTop

showGlyphInfos :: [Char] -> IO()
showGlyphInfos characters =
  flip withFreeType (showFailmessage "init freetype") $ \ftLib -> do
    flip (withNewFace ftLib "fonts/GibFontPlox.otf" 0) (showFailmessage "load font") $ \ftFace -> do
      forM_ characters (showGlyphInfo ftFace)
