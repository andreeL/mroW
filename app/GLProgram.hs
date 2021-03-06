{-# LANGUAGE RecordWildCards #-}

module GLProgram
  ( GLState
  , createGLState
  , reloadShaders
  , render
  ) where

import Common
import Control.Exception (catch, ErrorCall)
import Control.Monad (when, join)
import qualified Data.ByteString.Lazy as B (readFile, unpack)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Vector.Generic (convert)
import qualified Data.Vector.Storable as VS (fromList)
import FontBuilder (Font, createGUIFont)
import GHC.Exts (sortWith)
import Graphics.GL
import Graphics.UI.GLFW (Window, getFramebufferSize, swapBuffers)
import Linear (V3(..), V4(..))
import Logger (Logger)
import OpenGLHelpers
import Program (SceneObject(..), SceneState(..), GUIState(..))

sceneProgramSource = ("shaders/Scene.vert", "shaders/Scene.frag") :: (FilePath, FilePath)
postProcessingProgramSource = ("shaders/GUI.vert", "shaders/GUI.frag") :: (FilePath, FilePath)
catTextureSource = "data/catTexture.data" :: FilePath

data GLState = GLState {
  _dummyVAO :: GLuint,
  _emptyBO :: GLuint,
  _sceneTargetSize :: (GLsizei, GLsizei),
  _sceneTargetTexture :: GLuint,
  _sceneTargetBuffer :: GLuint,
  _sceneProgram :: GLSLProgram,
  _postProcessingProgram :: GLSLProgram,
  _font :: (Font, GLuint),
  _catTexture :: GLuint
}

createGLState :: Logger -> (Int, Int) -> IO GLState
createGLState logger (width, height) = do
  _dummyVAO <- createVAO
  _emptyBO <- createEmptyBO logger 0
  let _sceneTargetSize = (fromIntegral width, fromIntegral height)
  _sceneTargetTexture <- createSceneTargetTexture logger (fst _sceneTargetSize) (snd _sceneTargetSize)
  _sceneTargetBuffer <- createSceneTargetBuffer _sceneTargetTexture
  _sceneProgram <- createGLSLProgram logger sceneProgramSource
  _postProcessingProgram <- createGLSLProgram logger postProcessingProgramSource
  (font, ((textureWidth, textureHeight), textureData)) <- createGUIFont logger
  logger $ "Font texture size is: " ++ show textureWidth ++ "x" ++ show textureHeight
  fontTextureId <- createFontTexture logger (fromIntegral textureWidth) (fromIntegral textureHeight) (convert textureData)
  let _font = (font, fontTextureId)
  _catTexture <- loadTexture logger catTextureSource 128 128
  pure GLState{..}

loadTexture :: Logger -> FilePath -> GLsizei -> GLsizei -> IO GLuint
loadTexture logger textureSource width height = do
  catTextureData <- B.readFile textureSource
  createImageTexture logger width height (VS.fromList . B.unpack $ catTextureData)
  
reloadShaders :: Logger -> GLState -> IO ()
reloadShaders logger GLState{..} = catch (do
    recreateGLSLProgram logger _sceneProgram sceneProgramSource
    recreateGLSLProgram logger _postProcessingProgram postProcessingProgramSource
  ) failHandler
  where
    failHandler :: ErrorCall -> IO ()
    failHandler _ = logger "Failed to update all shaders!"

render :: Logger -> Double -> Window -> SceneState -> GUIState -> GLState -> IO GLState
render logger time window sceneState guiState glState = do
  glState' <-
    pure glState >>=
    renderScene logger time sceneState >>=
    renderGUI logger time window guiState
  swapBuffers window
  pure glState'

renderScene :: Logger -> Double -> SceneState -> GLState -> IO GLState
renderScene logger time sceneState glState@GLState{..} = do
  withFullscreenGLSLProgram logger _dummyVAO _emptyBO _sceneTargetBuffer _sceneTargetSize _sceneProgram $ \programId -> do
    let (V3 pX pY pZ) = _player sceneState
    setFloat logger programId "fTime" Nothing (realToFrac time)
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D (_catTexture)
    setInt logger programId "catTexture" Nothing 0
    setFloat3 logger programId "playerPosition" Nothing (realToFrac pX) (realToFrac pY) (realToFrac pZ)
    let closestObjects = fmap toGLObject $ buildClosestObjectList 100 (_objects sceneState)
    setFloat4Array logger programId "objects" Nothing (fmap realToFrac . concatMap toList $ closestObjects)
    let (V3 eyeX eyeY eyeZ, eyeRotation) = _camera sceneState
    setFloat3 logger programId "eyePosition" Nothing (realToFrac eyeX) (realToFrac eyeY) (realToFrac eyeZ)
    setMatrix33 logger programId "eyeRotation" Nothing (fmap realToFrac . join . fmap toList . toList $ eyeRotation)
  pure glState

renderGUI :: Logger -> Double -> Window -> GUIState -> GLState -> IO GLState
renderGUI logger time window guiState glState@GLState{..} = do
  (width, height) <- getFramebufferSize window
  withFullscreenGLSLProgram logger _dummyVAO _emptyBO 0 (fromIntegral width, fromIntegral height) _postProcessingProgram $ \programId -> do
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D _sceneTargetTexture
    setFloat logger programId "fTime" Nothing (realToFrac time)
    setInt logger programId "sceneTexture" Nothing 0
    glActiveTexture GL_TEXTURE1
    glBindTexture GL_TEXTURE_2D (snd _font)
    setInt logger programId "fontTexture" Nothing 1
    setInt logger programId "gPoints" Nothing (fromIntegral . _points $ guiState)
    setFloat logger programId "gEnergy" Nothing (_energy guiState)
    setInt logger programId "gGameOver" Nothing (if _showGameOver guiState then 1 else 0)
    setInt logger programId "gCurrentMenuOption" Nothing (fromIntegral . fromMaybe (-1) . _currentMenuOption $ guiState)
  pure glState

withFullscreenGLSLProgram logger dummyVAO emptyBO targetBuffer (width, height) program setupAction = do
  let aspectRatio = fromIntegral width / fromIntegral height
  let fov = if aspectRatio > 1.0 then (aspectRatio, 1.0) else (1.0, 1 / aspectRatio)
  
  -- for now we just treat the VAO as a GL requirement, and ignore it
  glBindVertexArray dummyVAO
  glBindVertexBuffer 0 emptyBO 0 0

  glBindFramebuffer GL_FRAMEBUFFER targetBuffer
  glDisable GL_DEPTH_TEST
  glViewport 0 0 width height

  let programId = getProgramId program
  glUseProgram programId
  setFloat2 logger programId "fFov" Nothing (fst fov) (snd fov)
  setupAction programId
  glDrawArrays GL_TRIANGLES 0 3

buildClosestObjectList :: Int -> [SceneObject] -> [SceneObject]
buildClosestObjectList len objects =
  let halfLength = len `div` 2
      indexes = [-halfLength..len - halfLength]
      zOrderedObjects = sortWith (\(SceneObject (V3 _ _ z) _) -> z) $ objects
  in build indexes zOrderedObjects
  where zDistance i (SceneObject (V3 _ _ z) _) = abs (fromIntegral i - z)
        build :: [Int] -> [SceneObject] -> [SceneObject]
        build [] _ = []
        build _ [] = []
        build (x:xs) all@(y:[]) = y:build xs all
        build (x:xs) all@(y:y2:ys) =
          if (zDistance x y < zDistance x y2)
            then y:build xs all
            else y2:build xs (y2:ys)

toGLObject :: SceneObject -> V4 Float
toGLObject SceneObject{..} = V4 x y z (fromIntegral _objectType)
  where V3 x y z = _objectPosition
