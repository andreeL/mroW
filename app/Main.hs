module Main where

import Behaviour (Behaviour(..))
import Common (DeltaTime(..))
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (fromMaybe)
import Menu (createMenu, createMenuState)
import GLProgram (GLState, createGLState, renderScene)
import qualified Graphics.UI.GLFW as GLFW
import Program (EventHandler, Event(..), Command(..))

defaultWindowSize = (960, 540) :: (Int, Int)
--defaultWindowSize = (1728, 972) :: (Int, Int)

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

type EventQueue = Control.Concurrent.STM.TQueue Event

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
        progGLState <- createGLState defaultWindowSize
        GLFW.setKeyCallback window (Just $ \window key scancode action mods -> do
          let pressed = action /= GLFW.KeyState'Released
          when pressed $ do
            when (key == GLFW.Key'Escape) $ GLFW.setWindowShouldClose window True
          atomically $ writeTQueue eventQueue $ KeyEvent key scancode action mods
          )
        GLFW.setCursorPosCallback window (Just $ \win x y -> do
          atomically $ writeTQueue eventQueue $ MouseEvent x y
          )
        time <- fmap (fromMaybe 0) GLFW.getTime
        runLoop time eventQueue (createMenu createMenuState) progGLState window
        GLFW.destroyWindow window

runLoop :: Double -> EventQueue -> EventHandler -> GLState -> GLFW.Window -> IO ()
runLoop previousTime eventQueue eventHandler glState window = do
  GLFW.pollEvents
  windowShouldClose <- GLFW.windowShouldClose window
  case windowShouldClose of
    True -> return ()
    False -> do
      time <- fmap (fromMaybe 0) GLFW.getTime
      let deltaTime = DeltaTime {getSeconds = realToFrac . max 0 . min 1 $ (time - previousTime)}

      let runCommand :: GLState -> Command -> IO GLState
          runCommand glState (RenderScene sceneInfo) = renderScene sceneInfo time window glState
          runCommand glState (Log log              ) = putStrLn ("Log: " ++ log) *> pure glState

      let handleEvent :: Event -> (GLState, EventHandler) -> IO (GLState, EventHandler)
          handleEvent event (glState, eventHandler) = do
            let (program, eventHandler') = getBehaviour eventHandler $ event
            glState' <- foldM runCommand glState program
            pure (glState', eventHandler')
         
      let handleQueuedEvents :: (GLState, EventHandler) -> IO (GLState, EventHandler)
          handleQueuedEvents (glState, eventHandler) = do
            maybeEvent <- atomically $ tryReadTQueue eventQueue
            case maybeEvent of
              Just event -> do
                (glState', eventHandler') <- handleEvent event (glState, eventHandler)
                handleQueuedEvents (glState', eventHandler')
              Nothing -> pure (glState, eventHandler)

      -- handle events
      (glState', eventHandler') <-
        pure (glState, eventHandler) >>=
        handleQueuedEvents >>=
        handleEvent (TickEvent time deltaTime) >>=
        handleEvent RenderEvent
      runLoop time eventQueue eventHandler' glState window
