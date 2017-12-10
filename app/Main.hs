{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad
import           Data.Bits ((.|.))
import           Data.Maybe
import           Game
import           Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import           OpenGLHelpers
import qualified Control.Exception

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
    postProcessingProgram :: GLSLProgram
    }

sceneProgramSource = ("shaders/Fullscreen_vert.glsl", "shaders/Main_frag.glsl") :: (FilePath, FilePath)
postProcessingProgramSource = ("shaders/Fullscreen_vert.glsl", "shaders/PostProcessing_frag.glsl") :: (FilePath, FilePath)
defaultWindowSize = (960, 540) :: (GLsizei, GLsizei) -- (1728, 1080)

createGLState :: IO GLState
createGLState = do
    dummyVAO <- createVAO
    emptyBO <- createEmptyBO 0
    let sceneTargetSize = defaultWindowSize
    sceneTargetTexture <- createSceneTargetTexture (fst sceneTargetSize) (snd sceneTargetSize)
    sceneTargetBuffer <- createSceneTargetBuffer sceneTargetTexture
    sceneProgram <- createGLSLProgram sceneProgramSource
    postProcessingProgram <- createGLSLProgram postProcessingProgramSource
    return GLState{..}

recreateGLSLPrograms :: GLState -> IO ()
recreateGLSLPrograms GLState{..} = Control.Exception.catch (do
        recreateGLSLProgram sceneProgram sceneProgramSource
        recreateGLSLProgram postProcessingProgram postProcessingProgramSource
    ) failHandler
    where
        failHandler :: Control.Exception.ErrorCall -> IO ()
        failHandler _ = putStrLn $ "Failed to update all shaders!"

main :: IO ()
main = do
    withGLFW $ do
        maybeWindow <- GLFW.createWindow (fromIntegral.fst $ defaultWindowSize) (fromIntegral.snd $ defaultWindowSize) "mroW" Nothing Nothing
        case maybeWindow of
            Nothing -> return ()
            Just window -> do
                GLFW.makeContextCurrent (Just window)
                GLFW.swapInterval 1 -- assuming this enables vsync (doesn't work on my machine)
                progGLState <- createGLState

                -- TODO: we probably want some proper event system
                -- these callbacks are just put here temporarily...
                GLFW.setKeyCallback window (Just $ \window key scancode action mods -> do
                    when (action == GLFW.KeyState'Pressed) $ do
                        when (key == GLFW.Key'Escape) $ GLFW.setWindowShouldClose window True
                        when (key == GLFW.Key'F1) $ recreateGLSLPrograms progGLState
                    )

                GLFW.setCursorPosCallback window (Just $ \win x y -> do
                    let sceneProgramId = getProgramId . sceneProgram $ progGLState
                    glUseProgram sceneProgramId
                    setFloat2 sceneProgramId "fMouse" Nothing (realToFrac x) (realToFrac y)
                    return ()
                    )

                runLoop progGLState window
                GLFW.destroyWindow window

runLoop :: GLState -> GLFW.Window -> IO ()
runLoop progGLState@GLState{..} window = do
    GLFW.pollEvents
    windowShouldClose <- GLFW.windowShouldClose window
    case windowShouldClose of
        True -> return ()
        False -> do
            maybeTime <- GLFW.getTime
            let time = fromMaybe 0 maybeTime

            -- handle events ?

            -- update state ?

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
                    setupAction programId
                    glDrawArrays GL_TRIANGLES 0 3

            -- render scene
            withFullscreenGLSLProgram sceneTargetBuffer sceneTargetSize sceneProgram $ \programId -> do
                return ()

            -- copy scene to main buffer with post processing
            withFullscreenGLSLProgram 0 (fromIntegral width, fromIntegral height) postProcessingProgram $ \programId -> do
                glActiveTexture GL_TEXTURE0
                glBindTexture GL_TEXTURE_2D sceneTargetTexture
                setInt programId "sceneTexture" Nothing 0
                setFloat2 programId "fMouse" Nothing 0 0

            -- post render
            GLFW.swapBuffers window
            runLoop progGLState window
