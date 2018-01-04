{-# LANGUAGE RecordWildCards #-}
module Main where

import Behaviour (Behaviour(..))
import Common (DeltaTime(..))
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe
import Game
import GameState
import GLProgram (GLState(..), createGLState, runProgram)
import qualified Graphics.UI.GLFW as GLFW

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

type GameEvent = GameState -> GameState
type EventQueue = Control.Concurrent.STM.TQueue GameEvent

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
        progGLState <- createGLState defaultWindowSize
        GLFW.setKeyCallback window (Just $ \window key scancode action mods -> do
          let pressed = action /= GLFW.KeyState'Released
          when pressed $ do
            when (key == GLFW.Key'Escape) $ GLFW.setWindowShouldClose window True
            when (key == GLFW.Key'F1) $ atomically $ writeTQueue eventQueue $ snd . setDirtyShadersFlag
            when (key == GLFW.Key'F2) $ atomically $ writeTQueue eventQueue $ snd . useNextCameraMode
            when (key == GLFW.Key'Space) $ atomically $ writeTQueue eventQueue $ snd . addPoints 1
          when (key == GLFW.Key'W) $ atomically $ writeTQueue eventQueue $ snd . setOrRemove varActionUp "" pressed
          when (key == GLFW.Key'A) $ atomically $ writeTQueue eventQueue $ snd . setOrRemove varActionLeft "" pressed
          when (key == GLFW.Key'S) $ atomically $ writeTQueue eventQueue $ snd . setOrRemove varActionDown "" pressed
          when (key == GLFW.Key'D) $ atomically $ writeTQueue eventQueue $ snd . setOrRemove varActionRight "" pressed
          )
        GLFW.setCursorPosCallback window (Just $ \win x y -> do
          atomically $ writeTQueue eventQueue $ snd . setMousePos (x, y)
          )
        time <- fmap (fromMaybe 0) GLFW.getTime
        runLoop time eventQueue (createGame createGameState) progGLState window
        GLFW.destroyWindow window

runLoop :: Double -> EventQueue -> Game -> GLState -> GLFW.Window -> IO ()
runLoop previousTime eventQueue game progGLState@GLState{..} window = do
  GLFW.pollEvents
  windowShouldClose <- GLFW.windowShouldClose window
  case windowShouldClose of
    True -> return ()
    False -> do
      time <- fmap (fromMaybe 0) GLFW.getTime
      let deltaTime = DeltaTime {getSeconds = realToFrac . max 0 . min 1 $ (time - previousTime)}
      let handleGameEvent :: Event -> Game -> IO Game
          handleGameEvent event game =
            let (program, game') = getBehaviour game $ event
            in runProgram (time, window, progGLState) program *> pure game'
         
      let handleQueuedEvents :: EventQueue -> Game -> IO Game
          handleQueuedEvents eventQueue game = do
            maybeEvent <- atomically $ tryReadTQueue eventQueue
            case maybeEvent of
              Just event -> do
                game' <- handleGameEvent (UpdateEvent event) game
                handleQueuedEvents eventQueue game'
              Nothing -> return game

      -- handle events
      game' <- handleQueuedEvents eventQueue game >>=
        handleGameEvent (TickEvent time deltaTime) >>=
        handleGameEvent RenderEvent
      runLoop time eventQueue game' progGLState window
