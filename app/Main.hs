{-# LANGUAGE RecordWildCards #-}
module Main where

import           Common (DeltaTime(..))
import           Control.Concurrent.STM
import qualified Control.Exception
import           Control.Monad
import           Data.Bits ((.|.))
import           Data.Maybe
import           Data.Foldable (toList)
import           Data.Vector.Generic (convert)
import           FontBuilder (Font, createHUDFont)
import           Game
import           GHC.Exts (sortWith)
import           Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L
import           OpenGLHelpers

errorCallback :: GLFW.ErrorCallback
errorCallback error description = do
    -- TODO!!! just log to error??
    putStrLn $ "Error: " ++ show error
    putStrLn description

withGLFW :: IO () -> IO ()
withGLFW program = do
    GLFW.setErrorCallback (Just errorCallback)
    initSuccessful <- GLFW.init
    when initSuccessful $ do
        program
        GLFW.terminate

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

type GameEvent = GameState -> GameState
type EventQueue = Control.Concurrent.STM.TQueue GameEvent

sceneProgramSource = ("shaders/Scene.vert", "shaders/Scene.frag") :: (FilePath, FilePath)
postProcessingProgramSource = ("shaders/HUD.vert", "shaders/HUD.frag") :: (FilePath, FilePath)
defaultWindowSize = (960, 540) :: (GLsizei, GLsizei)
--defaultWindowSize = (1728, 972) :: (GLsizei, GLsizei)

createGLState :: IO GLState
createGLState = do
    dummyVAO <- createVAO
    emptyBO <- createEmptyBO 0
    let sceneTargetSize = defaultWindowSize
    sceneTargetTexture <- createSceneTargetTexture (fst sceneTargetSize) (snd sceneTargetSize)
    sceneTargetBuffer <- createSceneTargetBuffer sceneTargetTexture
    sceneProgram <- createGLSLProgram sceneProgramSource
    postProcessingProgram <- createGLSLProgram postProcessingProgramSource
    (font, ((textureWidth, textureHeight), textureData)) <- createHUDFont
    putStrLn $ "Font texture size is: " ++ show textureWidth ++ "x" ++ show textureHeight
    fontTextureId <- createFontTexture (fromIntegral textureWidth) (fromIntegral textureHeight) (convert textureData)
    let _font = (font, fontTextureId)
    pure GLState{..}

recreateGLSLPrograms :: GLState -> IO ()
recreateGLSLPrograms GLState{..} = Control.Exception.catch (do
        recreateGLSLProgram sceneProgram sceneProgramSource
        recreateGLSLProgram postProcessingProgram postProcessingProgramSource
    ) failHandler
    where
        failHandler :: Control.Exception.ErrorCall -> IO ()
        failHandler _ = putStrLn $ "Failed to update all shaders!"

setOrRemove :: VariableName -> VariableValue -> Bool -> GameState -> ((), GameState)
setOrRemove variableName variableValue doSet =
    if (doSet == True)
        then setVariable variableName variableValue
        else removeVariable variableName

main :: IO ()
main = do
    withGLFW $ do
        maybeWindow <- GLFW.createWindow (fromIntegral.fst $ defaultWindowSize) (fromIntegral.snd $ defaultWindowSize) "mroW" Nothing Nothing
        case maybeWindow of
            Nothing -> return ()
            Just window -> do
                GLFW.makeContextCurrent (Just window)
                GLFW.swapInterval 1 -- assuming this enables vsync (doesn't work on my machine)

                eventQueue <- newTQueueIO
                progGLState <- createGLState

                GLFW.setKeyCallback window (Just $ \window key scancode action mods -> do
                    let pressed = action /= GLFW.KeyState'Released
                    when pressed $ do
                        when (key == GLFW.Key'Escape) $ GLFW.setWindowShouldClose window True
                        when (key == GLFW.Key'F1) $ atomically $ writeTQueue eventQueue $ snd . setDirtyShadersFlag
                        when (key == GLFW.Key'F2) $ atomically $ writeTQueue eventQueue $ snd . useNextCameraMode

                    when (key == GLFW.Key'W) $ atomically $ writeTQueue eventQueue $ snd . setOrRemove actionUp "" pressed
                    when (key == GLFW.Key'A) $ atomically $ writeTQueue eventQueue $ snd . setOrRemove actionLeft "" pressed
                    when (key == GLFW.Key'S) $ atomically $ writeTQueue eventQueue $ snd . setOrRemove actionDown "" pressed
                    when (key == GLFW.Key'D) $ atomically $ writeTQueue eventQueue $ snd . setOrRemove actionRight "" pressed
                    )

                GLFW.setCursorPosCallback window (Just $ \win x y -> do
                    atomically $ writeTQueue eventQueue $ snd . setMousePos (x, y)
                    )

                time <- fmap (fromMaybe 0) GLFW.getTime
                runLoop time eventQueue createGameState progGLState window
                GLFW.destroyWindow window

runQueuedEvents :: EventQueue -> GameState -> IO GameState
runQueuedEvents eventQueue gameState = do
    maybeEvent <- atomically $ tryReadTQueue eventQueue
    case maybeEvent of
        Just event -> runQueuedEvents eventQueue (event gameState)
        Nothing -> return gameState

-- this is just tempoary
testObjects :: Double -> [L.V4 Float]
testObjects time =
    let getZ offset =
            let z = ((realToFrac time) * 3 + offset)
             in z - (fromIntegral . round $ z / 60) * 60
     in [ L.V4 ( 1) ( 1) (getZ 0) 1.0,
          L.V4 (-1) ( 1) (getZ 4) 2.0,
          L.V4 ( 1) (-1) (getZ 8) 2.0,
          L.V4 (-1) (-1) (getZ 12) 1.0,
          L.V4 ( 0) ( 0) (getZ 16) 1.0,
          L.V4 ( 1) ( 1) (getZ 20) 1.0,
          L.V4 (-1) ( 1) (getZ 24) 2.0,
          L.V4 (-1) ( 1) (getZ 28) 2.0,
          L.V4 (-1) ( 1) (getZ 32) 2.0,
          L.V4 ( 1) ( 1) (getZ 36) 1.0,
          L.V4 (-1) ( 1) (getZ 40) 2.0,
          L.V4 ( 1) (-1) (getZ 44) 2.0,
          L.V4 (-1) (-1) (getZ 48) 2.0,
          L.V4 ( 0) ( 0) (getZ 52) 1.0
        ]

buildClosestObjectList :: Int -> [L.V4 Float] -> [L.V4 Float]
buildClosestObjectList len objects =
    let halfLength = len `div` 2
        indexes = [-halfLength..len - halfLength]
        zOrderedObjects = sortWith (\(L.V4 _ _ z _) -> z) $ objects
     in build indexes zOrderedObjects
    where zDistance i (L.V4 _ _ z _) = abs (fromIntegral i - z)
          build :: [Int] -> [L.V4 Float] -> [L.V4 Float]
          build [] _ = []
          build (x:xs) all@(y:[]) = y:build xs all
          build (x:xs) all@(y:y2:ys) =
            if (zDistance x y < zDistance x y2)
                then y:build xs all
                else y2:build xs (y2:ys)
          
runLoop :: Double -> EventQueue -> GameState -> GLState -> GLFW.Window -> IO ()
runLoop previousTime eventQueue gameState progGLState@GLState{..} window = do
    GLFW.pollEvents
    windowShouldClose <- GLFW.windowShouldClose window
    case windowShouldClose of
        True -> return ()
        False -> do
            time <- fmap (fromMaybe 0) GLFW.getTime
            let deltaTime = DeltaTime {getSeconds = realToFrac . max 0 . min 1 $ (time - previousTime)}

            -- handle events
            ((eyePosition, eyeRotation), playerPosition, gameState') <- fmap (run time deltaTime) $ runQueuedEvents eventQueue gameState

            -- update state ?
            let (shadersAreDirty, gameState'') = extractDirtyShadersFlag gameState'
            when (shadersAreDirty) $ do
                recreateGLSLPrograms progGLState
            let (mouseX, mouseY) = getMousePos gameState''

            -- pre render
            (width, height) <- GLFW.getFramebufferSize window
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
                    let L.V3 eyeX eyeY eyeZ = eyePosition
                    setFloat3 programId "eyePosition" Nothing (realToFrac eyeX) (realToFrac eyeY) (realToFrac eyeZ)
                    setMatrix33 programId "eyeRotation" Nothing (fmap realToFrac . join . fmap toList . toList $ eyeRotation)
                    setupAction programId
                    glDrawArrays GL_TRIANGLES 0 3

            -- render scene
            withFullscreenGLSLProgram sceneTargetBuffer sceneTargetSize sceneProgram $ \programId -> do
                let L.V3 pX pY pZ = playerPosition
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

            -- post render
            GLFW.swapBuffers window
            runLoop time eventQueue gameState'' progGLState window
