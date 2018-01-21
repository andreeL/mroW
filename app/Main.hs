{-# LANGUAGE RecordWildCards #-}

module Main where

import Behaviour (Behaviour(..))
import Common (DeltaTime(..))
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (fromMaybe)
import Menu (createMenu, createMenuState)
import GLProgram (GLState, createGLState, reloadShaders, render)
import qualified Graphics.UI.GLFW as GLFW
import Program (EventHandler, Event(..), SceneState, createSceneState, GUIState, createGUIState, Command(..))

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

data ProgramState = ProgramState {
  _sceneState :: SceneState,
  _guiState :: GUIState,
  _glState :: GLState
}

createProgramState :: IO ProgramState
createProgramState = do
  glState <- createGLState defaultWindowSize
  pure ProgramState{
    _sceneState = createSceneState,
    _guiState = createGUIState,
    _glState = glState
  }

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
        programState <- createProgramState
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
        runLoop time eventQueue (createMenu createMenuState) programState window
        GLFW.destroyWindow window

runLoop :: Double -> EventQueue -> EventHandler -> ProgramState -> GLFW.Window -> IO ()
runLoop previousTime eventQueue eventHandler programState window = do
  GLFW.pollEvents
  windowShouldClose <- GLFW.windowShouldClose window
  case windowShouldClose of
    True -> return ()
    False -> do
      time <- fmap (fromMaybe 0) GLFW.getTime
      let deltaTime = DeltaTime {getSeconds = realToFrac . max 0 . min 1 $ (time - previousTime)}

      let runCommand :: ProgramState -> Command -> IO ProgramState
          runCommand programState@ProgramState{..} (ReloadShaders) = reloadShaders _glState *> pure programState
          runCommand programState@ProgramState{..} (UpdateScene f) = pure ProgramState {_sceneState = f _sceneState, _guiState = _guiState, _glState = _glState}
          runCommand programState@ProgramState{..} (UpdateGUI f  ) = pure ProgramState {_sceneState = _sceneState, _guiState = f _guiState, _glState = _glState}
          runCommand programState@ProgramState{..} (Log log      ) = putStrLn ("Log: " ++ log) *> pure programState

      let handleEvent :: Event -> (ProgramState, EventHandler) -> IO (ProgramState, EventHandler)
          handleEvent event (programState, eventHandler) = do
            let (program, eventHandler') = getBehaviour eventHandler $ event
            programState' <- foldM runCommand programState program
            pure (programState', eventHandler')
         
      let handleQueuedEvents :: (ProgramState, EventHandler) -> IO (ProgramState, EventHandler)
          handleQueuedEvents (programState, eventHandler) = do
            maybeEvent <- atomically $ tryReadTQueue eventQueue
            case maybeEvent of
              Just event -> do
                (programState', eventHandler') <- handleEvent event (programState, eventHandler)
                handleQueuedEvents (programState', eventHandler')
              Nothing -> pure (programState, eventHandler)

      -- handle events
      (ProgramState sceneState guiState glState, eventHandler') <-
        pure (programState, eventHandler) >>=
        handleQueuedEvents >>=
        handleEvent (TickEvent time deltaTime) >>=
        handleEvent UpdateRenderStates

      glState' <- render time window sceneState guiState glState
      runLoop time eventQueue eventHandler' (ProgramState sceneState guiState glState') window
