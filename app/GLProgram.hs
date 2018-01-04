{-# LANGUAGE RecordWildCards #-}
module GLProgram
  ( GLState(..)
  , createGLState
  , runProgram
  ) where

import Common
import Control.Exception (catch, ErrorCall)
import Control.Monad (when, join)
import Data.Foldable (toList)
import Data.Vector.Generic (convert)
import FontBuilder (Font, createHUDFont)
import GHC.Exts (sortWith)
import Graphics.GL
import Graphics.UI.GLFW (Window, getFramebufferSize, swapBuffers)
import Linear (V3(..), V4(..))
import OpenGLHelpers
import Program (Program(..), RenderInfo(..))

sceneProgramSource = ("shaders/Scene.vert", "shaders/Scene.frag") :: (FilePath, FilePath)
postProcessingProgramSource = ("shaders/HUD.vert", "shaders/HUD.frag") :: (FilePath, FilePath)

data GLState = GLState {
  dummyVAO :: GLuint,
  emptyBO :: GLuint,
  sceneTargetSize :: (GLsizei, GLsizei),
  sceneTargetTexture :: GLuint,
  sceneTargetBuffer :: GLuint,
  sceneProgram :: GLSLProgram,
  postProcessingProgram :: GLSLProgram,
  _font :: (Font, GLuint)
}

createGLState :: (Int, Int) -> IO GLState
createGLState (width, height) = do
  dummyVAO <- createVAO
  emptyBO <- createEmptyBO 0
  let sceneTargetSize = (fromIntegral width, fromIntegral height)
  sceneTargetTexture <- createSceneTargetTexture (fst sceneTargetSize) (snd sceneTargetSize)
  sceneTargetBuffer <- createSceneTargetBuffer sceneTargetTexture
  sceneProgram <- createGLSLProgram sceneProgramSource
  postProcessingProgram <- createGLSLProgram postProcessingProgramSource
  (font, ((textureWidth, textureHeight), textureData)) <- createHUDFont
  putStrLn $ "Font texture size is: " ++ show textureWidth ++ "x" ++ show textureHeight
  fontTextureId <- createFontTexture (fromIntegral textureWidth) (fromIntegral textureHeight) (convert textureData)
  let _font = (font, fontTextureId)
  pure GLState{..}

runProgram :: (Double, Window, GLState) -> Program -> IO ()
runProgram _ NoOp = pure ()
runProgram (time, window, progGLState@GLState{..}) (Render renderInfo) = do
  when (_shadersAreDirty renderInfo) $ do
    recreateGLSLPrograms progGLState
  let (mouseX, mouseY) = _mousePos renderInfo

  -- pre render
  (width, height) <- getFramebufferSize window
  let aspectRatio = fromIntegral width / fromIntegral height
  let fov = if aspectRatio > 1.0 then (aspectRatio, 1.0) else (1.0, 1 / aspectRatio)
  let withFullscreenGLSLProgram targetBuffer (width, height) program setupAction = do
        -- for now we just treat the VAO as a GL requirement, and ignore it
        glBindVertexArray dummyVAO
        glBindVertexBuffer 0 emptyBO 0 0

        glBindFramebuffer GL_FRAMEBUFFER targetBuffer
        glDisable GL_DEPTH_TEST
        glViewport 0 0 width height

        let programId = getProgramId program
        glUseProgram programId
        setFloat programId "fTime" Nothing (realToFrac time)
        setFloat2 programId "fFov" Nothing (fst fov) (snd fov)
        setFloat2 programId "fMouse" Nothing (realToFrac mouseX) (realToFrac mouseY)
        let (V3 eyeX eyeY eyeZ, eyeRotation) = _camera renderInfo
        setFloat3 programId "eyePosition" Nothing (realToFrac eyeX) (realToFrac eyeY) (realToFrac eyeZ)
        setMatrix33 programId "eyeRotation" Nothing (fmap realToFrac . join . fmap toList . toList $ eyeRotation)
        setupAction programId
        glDrawArrays GL_TRIANGLES 0 3

  -- render scene
  withFullscreenGLSLProgram sceneTargetBuffer sceneTargetSize sceneProgram $ \programId -> do
    let (V3 pX pY pZ) = _player renderInfo
    setFloat3 programId "playerPosition" Nothing (realToFrac pX) (realToFrac pY) (realToFrac pZ)
    let closestObjects = buildClosestObjectList 100 (testObjects time)
    setFloat4Array programId "objects" Nothing (fmap realToFrac . concatMap toList $ closestObjects)

  -- copy scene to main buffer with post processing
  withFullscreenGLSLProgram 0 (fromIntegral width, fromIntegral height) postProcessingProgram $ \programId -> do
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D sceneTargetTexture
    setInt programId "sceneTexture" Nothing 0
    glActiveTexture GL_TEXTURE1
    glBindTexture GL_TEXTURE_2D (snd _font)
    setInt programId "fontTexture" Nothing 1
    setFloat2 programId "fMouse" Nothing 0 0
    setInt programId "gPoints" Nothing (fromIntegral . _points $ renderInfo)

  -- post render
  swapBuffers window

  --pure progGLState

recreateGLSLPrograms :: GLState -> IO ()
recreateGLSLPrograms GLState{..} = catch (do
    recreateGLSLProgram sceneProgram sceneProgramSource
    recreateGLSLProgram postProcessingProgram postProcessingProgramSource
  ) failHandler
  where
    failHandler :: ErrorCall -> IO ()
    failHandler _ = putStrLn $ "Failed to update all shaders!"
  
-- this is just tempoary
testObjects :: Double -> [V4 Float]
testObjects time =
  let getZ offset =
        let z = ((realToFrac time) * 3 + offset)
        in z - (fromIntegral . round $ z / 60) * 60
  in [ V4 ( 1) ( 1) (getZ 0) 1.0,
       V4 (-1) ( 1) (getZ 4) 2.0,
       V4 ( 1) (-1) (getZ 8) 2.0,
       V4 (-1) (-1) (getZ 12) 1.0,
       V4 ( 0) ( 0) (getZ 16) 1.0,
       V4 ( 1) ( 1) (getZ 20) 1.0,
       V4 (-1) ( 1) (getZ 24) 2.0,
       V4 (-1) ( 1) (getZ 28) 2.0,
       V4 (-1) ( 1) (getZ 32) 2.0,
       V4 ( 1) ( 1) (getZ 36) 1.0,
       V4 (-1) ( 1) (getZ 40) 2.0,
       V4 ( 1) (-1) (getZ 44) 2.0,
       V4 (-1) (-1) (getZ 48) 2.0,
       V4 ( 0) ( 0) (getZ 52) 1.0
  ]

buildClosestObjectList :: Int -> [V4 Float] -> [V4 Float]
buildClosestObjectList len objects =
  let halfLength = len `div` 2
      indexes = [-halfLength..len - halfLength]
      zOrderedObjects = sortWith (\(V4 _ _ z _) -> z) $ objects
  in build indexes zOrderedObjects
  where zDistance i (V4 _ _ z _) = abs (fromIntegral i - z)
        build :: [Int] -> [V4 Float] -> [V4 Float]
        build [] _ = []
        build (x:xs) all@(y:[]) = y:build xs all
        build (x:xs) all@(y:y2:ys) =
          if (zDistance x y < zDistance x y2)
            then y:build xs all
            else y2:build xs (y2:ys)