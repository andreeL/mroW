{-# LANGUAGE RecordWildCards #-}

module Game (
  Event(..),
  Game,
  createGame,
) where

import Behaviour (Behaviour(..), bScanSplit)
import Camera (Camera, CameraInput(..))
import Common
import Data.Maybe (isJust)
import GameState
import Lens.Micro.Platform
import Player (Player, PlayerInput(..))
import Program (Program(..), SceneInfo(..))

data Event = UpdateEvent (GameState -> GameState)
           | TickEvent Double DeltaTime
           | RenderEvent

type Game = Behaviour Event Program
type ProgramBuilder = GameState -> (Program, GameState)

createGame :: GameState -> Game
createGame state = bScanSplit (flip handleEvent) state

handleEvent :: Event -> ProgramBuilder
handleEvent (UpdateEvent f)            = handleUpdateEvent f
handleEvent (TickEvent time deltaTime) = handleTickEvent time deltaTime
handleEvent (RenderEvent)              = handleRenderEvent

handleUpdateEvent :: (GameState -> GameState) -> ProgramBuilder
handleUpdateEvent f state = (NoOp, f state)

handleTickEvent :: Double -> DeltaTime -> ProgramBuilder
handleTickEvent time deltaTime state = let
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

  in (NoOp, GameState{..})

handleRenderEvent :: ProgramBuilder
handleRenderEvent state =
  let (_shadersAreDirty, state') = extractDirtyShadersFlag state
      _camera = state' ^. lastCameraPlacement
      _player = state' ^. lastPlayerPosition
      _mousePos = getMousePos state'
      _points = getPoints state'
  in (RenderScene SceneInfo{..}, state')
