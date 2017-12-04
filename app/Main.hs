module Main where

import           Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import           Lib
import           Graphics.GL
import           Data.Bits ((.|.))

errorCallback :: GLFW.ErrorCallback
errorCallback error description = do
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
                GLFW.swapInterval 1 -- assuming this enables vsync
                runLoop window
                GLFW.destroyWindow window

runLoop :: GLFW.Window -> IO ()
runLoop window = do
    GLFW.pollEvents
    windowShouldClose <- GLFW.windowShouldClose window
    case windowShouldClose of
        True -> return ()
        False -> do
            -- update state ?
            -- handle input ?
            maybeTime <- GLFW.getTime
            case maybeTime of
                Nothing -> return ()
                Just time -> return () -- update the scene or something here...

            -- pre render
            (width, height) <- GLFW.getFramebufferSize window
            let aspectRatio = fromIntegral width / fromIntegral height
            glViewport 0 0 (fromIntegral width) (fromIntegral height)
            glClearColor 1.0 0.7 0.4 1.0
            glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
            
            -- render ?

            -- post render
            GLFW.swapBuffers window
            runLoop window

keyCallback :: GLFW.KeyCallback
keyCallback window key scancode action mods = do
    when (key == GLFW.Key'Escape) $
        GLFW.setWindowShouldClose window True

    -- printing key input for testing
    putStrLn $ show key ++ " - " ++ show scancode ++ " - " ++ show action ++ " - " ++ show mods
