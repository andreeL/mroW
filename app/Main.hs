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

main :: IO ()
main = do
    withGLFW $ do
        maybeWindow <- GLFW.createWindow 960 540 "mroW" Nothing Nothing
        case maybeWindow of
            Nothing -> return ()
            Just window -> do
                GLFW.makeContextCurrent (Just window)
                GLFW.setKeyCallback window (Just keyCallback)
                GLFW.swapInterval 1 -- assuming this enables vsync (doesn't work on my machine)
                dummyVAO <- createVAO
                emptyBO <- createEmptyBO 0
                mainProgram <- createProgram "shaders/Fullscreen_vert.glsl" "shaders/Main_frag.glsl"
                runLoop dummyVAO emptyBO mainProgram window
                GLFW.destroyWindow window

runLoop :: GLuint -> GLuint -> GLuint -> GLFW.Window -> IO ()
runLoop dummyVAO emptyBO mainProgram window = do
    GLFW.pollEvents
    windowShouldClose <- GLFW.windowShouldClose window
    case windowShouldClose of
        True -> return ()
        False -> do
            maybeTime <- GLFW.getTime
            let time = fromMaybe 0 maybeTime

            -- handle input ?

            -- update state ?

            -- pre render
            (width, height) <- GLFW.getFramebufferSize window
            let aspectRatio = fromIntegral width / fromIntegral height
            glViewport 0 0 (fromIntegral width) (fromIntegral height)
            --glClearColor 1.0 0.7 0.4 1.0
            --glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
            
            -- render
            -- for now we just treat the VAO as a GL requirement, and ignore it
            glBindVertexArray dummyVAO
            glBindVertexBuffer 0 emptyBO 0 0
            glUseProgram mainProgram
            setFloat mainProgram "fTime" Nothing (realToFrac time)
            glDrawArrays GL_TRIANGLES 0 3

            -- post render
            GLFW.swapBuffers window
            runLoop dummyVAO emptyBO mainProgram window

renderFullScreenQuad :: IO ()
renderFullScreenQuad = undefined

keyCallback :: GLFW.KeyCallback
keyCallback window key scancode action mods = do
    when (key == GLFW.Key'Escape) $
        GLFW.setWindowShouldClose window True

    -- printing key input for testing
    putStrLn $ show key ++ " - " ++ show scancode ++ " - " ++ show action ++ " - " ++ show mods
