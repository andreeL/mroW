{-# LANGUAGE RecordWildCards #-}
module Tools where

import Control.Arrow (first)
import Data.Foldable (foldl')
import qualified Data.Map as Map (lookup)
import FontBuilder

data Align = AlignLeft | AlignCenter

-- This is just some quick and dirty hack to help getting a prototype menu up and running fast! if it's performant enough I just might end up sticking with it... =)

getGUIFont :: IO Font
getGUIFont = fst <$> createGUIFont

generateGLSLCodeFromText :: Align -> Font -> String -> String
generateGLSLCodeFromText align font text =
    let (width, glyphs) = first fromIntegral (createText font text)
        xStart = case align of
                AlignLeft -> 0
                AlignCenter -> -width / 2
        glslCode (char, (offsetX, offsetY)) code =
            "vec4(position + vec2" ++ show (xStart + (fromIntegral offsetX), offsetY) ++ " * size, size2, " ++ show (fromEnum char) ++ "), // " ++ show char ++ "\n" ++ code
        glyphVectorElements = foldr glslCode "" glyphs
        noOfGlyphs = show . length $ glyphs
        findAndReturnColor = "float distance = 1;\n"
            ++ "for (int i = 0; i < " ++ noOfGlyphs ++ "; ++i)\n"
            ++ "distance = min(distance, getDistanceToChar(text[i]));\n"
            ++ "return getColor(distance);\n"
    in "vec4 getSomeText()\n"
        ++ "{\n"
        ++ "const vec2 position = vec2(?, ?);\n" -- position of the text
        ++ "const float size = ?;\n" -- scaling of the character offsets
        ++ "const float size2 = ?;\n" -- scaling of the characters
        ++ "const vec2 downLeft = vec2" ++ show (xStart - 4, -8) ++ " * size + position;\n" -- these values are just approximations (guesses), to pick values with exact fit we would have to do min-max on glyph sizes
        ++ "const vec2 upRight = vec2" ++ show (xStart + width + 4, 24) ++ " * size + position;\n"
        ++ "if (screenUV.x < downLeft.x || screenUV.x > upRight.x || screenUV.y < downLeft.y || screenUV.y > upRight.y)\n"
        ++ "return vec4(0.0);\n"
        ++ "const vec4 text[" ++ noOfGlyphs ++  "] = vec4[" ++ noOfGlyphs ++ "]( // " ++ text ++ "\n"
        ++ glyphVectorElements
        ++ ");\n"
        ++ findAndReturnColor
        ++ "}\n"

createText :: Font -> String -> (Int, [(Char, (Int, Int))])
createText font text = foldl' addCharacterPosition (0, []) text
  where
    addCharacterPosition (x, glyphOffsets) char = case (Map.lookup char font) of
      Just Glyph{..} -> (x + _advance, glyphOffsets ++ [(char, first ((+x)) $ _bearing)])
      Nothing -> (x, glyphOffsets)
