{-# LANGUAGE RecordWildCards #-}

module Game (
  createGame
) where

import Behaviour (Behaviour(..), bScanSplit)
import Camera (Camera, CameraInput(..))
import Common
import Data.Maybe (isJust)
import GameState
import Lens.Micro.Platform
import Player (Player, PlayerInput(..))
import Program (EventHandler, Event(..), SceneState(..), GUIState(..), Command(..))
import qualified Graphics.UI.GLFW as GLFW

type ProgramBuilder = GameState -> ([Command], GameState)

createGame :: GameState -> EventHandler
createGame = bScanSplit (flip handleEvent)

handleEvent :: Event -> ProgramBuilder
handleEvent (KeyEvent key scancode action mods) = handleKeyEvent key scancode action mods
handleEvent (MouseEvent x y)                    = handleMouseEvent (x, y)
handleEvent (TickEvent time deltaTime)          = handleTickEvent time deltaTime
handleEvent (UpdateRenderStates)                = handleUpdateRenderStatesEvent

setOrRemove :: VariableName -> VariableValue -> Bool -> GameState -> GameState
setOrRemove variableName variableValue doSet =
  snd . if (doSet == True)
    then setVariable variableName variableValue
    else removeVariable variableName

handleKeyEvent :: GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> ProgramBuilder
handleKeyEvent key scancode action mods state =
  let keyDown = action /= GLFW.KeyState'Released
      state' = case key of
        GLFW.Key'W -> setOrRemove varActionUp "" keyDown state
        GLFW.Key'A -> setOrRemove varActionLeft "" keyDown state
        GLFW.Key'S -> setOrRemove varActionDown "" keyDown state
        GLFW.Key'D -> setOrRemove varActionRight "" keyDown state
        _ -> state

      state'' = if action == GLFW.KeyState'Pressed then
        case key of
          GLFW.Key'F2 -> snd . useNextCameraMode $ state'
          GLFW.Key'Space -> snd . addPoints 1 $ state'
          _ -> state'
        else state'
  
  in ([], state'')

handleMouseEvent :: (Double, Double) -> ProgramBuilder
handleMouseEvent mousePos state = ([], snd . setMousePos mousePos $ state)

handleUpdateEvent :: (GameState -> GameState) -> ProgramBuilder
handleUpdateEvent f state = ([], f state)

handleTickEvent :: Double -> DeltaTime -> ProgramBuilder
handleTickEvent time deltaTime state = let
  isSet var = isJust . getVariable var $ state
  
  playerInput = PlayerInput {
    _time = time,
    _deltaTime = deltaTime,
    _moveUp = isJust . getVariable varActionUp $ state,
    _moveLeft = isJust . getVariable varActionLeft $ state,
    _moveDown = isJust . getVariable varActionDown $ state,
    _moveRight = isJust . getVariable varActionRight $ state
  }
  (_lastPlayerPosition, _player) = getBehaviour (state ^. player) playerInput

  cameraInput = CameraInput {
    _time = time,
    _deltaTime = deltaTime,
    _mouseXY = getMousePos state,
    _target = _lastPlayerPosition
  }
  _variables = state ^. variables
  (_lastCameraPlacement, _camera) = getBehaviour (state ^. camera) cameraInput

  in ([], GameState{..})

handleUpdateRenderStatesEvent :: ProgramBuilder
handleUpdateRenderStatesEvent state =
  let sceneState = SceneState { _camera = state ^. lastCameraPlacement,
                                _player = state ^. lastPlayerPosition
                              }
      guiState = GUIState { _points = getPoints state }
      commands = [ UpdateScene (const sceneState),
                   UpdateGUI (const guiState)
                 ]
  in (commands, state)
