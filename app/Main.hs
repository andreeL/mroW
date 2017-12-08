{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad
import           Data.Bits ((.|.))
import           Data.Maybe
import           Lib
import           Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
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
    sceneProgram :: GLuint,
    postProcessingProgram :: GLuint
    }

createGLState :: IO GLState
createGLState = do
    dummyVAO <- createVAO
    emptyBO <- createEmptyBO 0
    let sceneTargetSize = (960, 540)
    sceneTargetTexture <- createSceneTargetTexture (fst sceneTargetSize) (snd sceneTargetSize)
    sceneTargetBuffer <- createSceneTargetBuffer sceneTargetTexture
    sceneProgram <- createProgram "shaders/Fullscreen_vert.glsl" "shaders/Main_frag.glsl"
    postProcessingProgram <- createProgram "shaders/Fullscreen_vert.glsl" "shaders/PostProcessing_frag.glsl"
    return GLState{..}

main :: IO ()
main = do
    withGLFW $ do
        maybeWindow <- GLFW.createWindow 960 540 "mroW" Nothing Nothing
        case maybeWindow of
            Nothing -> return ()
            Just window -> do
                GLFW.makeContextCurrent (Just window)
                GLFW.swapInterval 1 -- assuming this enables vsync (doesn't work on my machine)
                progGLState <- createGLState

                -- TODO: we probably want some proper event system
                GLFW.setKeyCallback window (Just keyCallback)
                GLFW.setCursorPosCallback window (Just $ \win x y -> do
                    let program = sceneProgram progGLState
                    glUseProgram program
                    setFloat2 program "fMouse" Nothing (realToFrac x) (realToFrac y)
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
            glDisable GL_DEPTH_TEST

            -- for now we just treat the VAO as a GL requirement, and ignore it
            glBindVertexArray dummyVAO
            glBindVertexBuffer 0 emptyBO 0 0

            let setGenericShaderUniforms program = do
                    setFloat program "fTime" Nothing (realToFrac time)
                    setFloat2 program "fFov" Nothing (fst fov) (snd fov)

            -- render scene
            glBindFramebuffer GL_FRAMEBUFFER sceneTargetBuffer
            glViewport 0 0 (fst sceneTargetSize) (snd sceneTargetSize)
            glUseProgram sceneProgram
            setGenericShaderUniforms sceneProgram
            glDrawArrays GL_TRIANGLES 0 3

            -- copy scene to main buffer with post processing
            glBindFramebuffer GL_FRAMEBUFFER 0
            glViewport 0 0 (fromIntegral width) (fromIntegral height)
            glUseProgram postProcessingProgram
            setGenericShaderUniforms postProcessingProgram
            setInt postProcessingProgram "sceneTexture" Nothing 0
            glActiveTexture GL_TEXTURE0
            glBindTexture GL_TEXTURE_2D sceneTargetTexture
            glDrawArrays GL_TRIANGLES 0 3
            
            -- post render
            GLFW.swapBuffers window
            runLoop progGLState window

renderFullScreenQuad :: IO ()
renderFullScreenQuad = undefined

keyCallback :: GLFW.KeyCallback
keyCallback window key scancode action mods = do
    when (key == GLFW.Key'Escape) $
        GLFW.setWindowShouldClose window True

    -- printing key input for testing
    putStrLn $ show key ++ " - " ++ show scancode ++ " - " ++ show action ++ " - " ++ show mods
