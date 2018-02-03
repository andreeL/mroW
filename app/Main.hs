{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Behaviour (Behaviour(..))
import Common (DeltaTime(..))
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (fromMaybe)
import Lens.Micro.Platform
import Menu (createMenu, createMenuState)
import Mixer (startMixer, SoundOutput(..), AddSoundWave, StopMixer, pianoKey)
import GLProgram (GLState, createGLState, reloadShaders, render)
import qualified Graphics.UI.GLFW as GLFW
import Program (EventHandler, Event(..), SceneState, createSceneState, GUIState, createGUIState, Sound(..), Command(..))

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

data ProgramSettings = ProgramSettings{
  _settingNoOfSamplesInSoundBuffer :: Int,
  _settingSoundOutput :: SoundOutput
}

data ProgramState = ProgramState {
  _soundOutput :: SoundOutput,
  _addSoundWave :: AddSoundWave,
  _stopMixer :: StopMixer,
  _sceneState :: SceneState,
  _guiState :: GUIState,
  _glState :: GLState
}

makeLenses ''ProgramState

createProgramState :: ProgramSettings -> IO ProgramState
createProgramState ProgramSettings{..} = do
  (_addSoundWave, _stopMixer) <- startMixer _settingNoOfSamplesInSoundBuffer _settingSoundOutput
  glState <- createGLState defaultWindowSize
  pure ProgramState{
    _soundOutput = _settingSoundOutput,
    _addSoundWave = _addSoundWave,
    _stopMixer = _stopMixer,
    _sceneState = createSceneState,
    _guiState = createGUIState,
    _glState = glState
  }

-- destroyProgramState is only really needed for GHCi
destroyProgramState :: ProgramState -> IO ()
destroyProgramState ProgramState{..} = do
  -- TODO: do we want to cleanup GPU stuff as well? for now destroying the window is enough
  _stopMixer

main :: IO ()
main = runProgram ProgramSettings {
  _settingNoOfSamplesInSoundBuffer = 2048,
  _settingSoundOutput = SoundOutput {
    _sampleRate = 48000,
    _noOfChannels = 2
  }
}

-- ghciMain can run with lower settings to decrease the startup time, and to reduce lagg
ghciMain :: IO ()
ghciMain = runProgram ProgramSettings {
  _settingNoOfSamplesInSoundBuffer = 4096,
  _settingSoundOutput = SoundOutput {
    _sampleRate = 22500,
    _noOfChannels = 1
  }
}
  
runProgram :: ProgramSettings -> IO ()
runProgram programSettings = do
  withGLFW $ do
    maybeWindow <- GLFW.createWindow (fromIntegral.fst $ defaultWindowSize) (fromIntegral.snd $ defaultWindowSize) "mroW" Nothing Nothing
    case maybeWindow of
      Nothing -> return ()
      Just window -> do
        GLFW.makeContextCurrent (Just window)
        GLFW.swapInterval 1 -- assuming this enables vsync (doesn't work on my machine)
        eventQueue <- newTQueueIO
        
        programState <- createProgramState programSettings
        GLFW.setKeyCallback window (Just $ \window key scancode action mods -> do
          atomically $ writeTQueue eventQueue $ KeyEvent key scancode action mods
          )
        GLFW.setCursorPosCallback window (Just $ \win x y -> do
          atomically $ writeTQueue eventQueue $ MouseEvent x y
          )
        time <- fmap (fromMaybe 0) GLFW.getTime
        programState' <- runLoop time eventQueue (createMenu createMenuState) programState window
        destroyProgramState programState'
        GLFW.destroyWindow window

runLoop :: Double -> EventQueue -> EventHandler -> ProgramState -> GLFW.Window -> IO ProgramState
runLoop previousTime eventQueue eventHandler programState window = do
  GLFW.pollEvents
  windowShouldClose <- GLFW.windowShouldClose window
  case windowShouldClose of
    True -> pure programState
    False -> do
      time <- fmap (fromMaybe 0) GLFW.getTime
      let deltaTime = DeltaTime {getSeconds = realToFrac . max 0 . min 1 $ (time - previousTime)}

      let runCommand :: ProgramState -> Command -> IO ProgramState
          runCommand programState@ProgramState{..} (Terminate         ) = GLFW.setWindowShouldClose window True *> pure programState
          runCommand programState@ProgramState{..} (MarkShadersAsDirty) = reloadShaders _glState *> pure programState
          runCommand programState@ProgramState{..} (UpdateScene f     ) = pure $ programState & sceneState %~ f
          runCommand programState@ProgramState{..} (UpdateGUI f       ) = pure $ programState & guiState %~ f
          runCommand programState@ProgramState{..} (PlaySound sound   ) = playSound _soundOutput _addSoundWave sound *> pure programState
          runCommand programState@ProgramState{..} (Log log           ) = putStrLn ("Log: " ++ log) *> pure programState

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
      (programState', eventHandler') <-
        pure (programState, eventHandler) >>=
        handleQueuedEvents >>=
        handleEvent (TickEvent time deltaTime) >>=
        handleEvent UpdateRenderStates

      glState' <- render time window (programState' ^. sceneState) (programState' ^. guiState) (programState' ^. glState)
      runLoop time eventQueue eventHandler' (programState' & glState .~ glState') window

playSound :: SoundOutput -> AddSoundWave -> Sound -> IO()
playSound soundOuput addSoundWave sound =
  case sound of
    Piano key -> addSoundWave $ pianoKey soundOuput (40 + (key `mod`10))
