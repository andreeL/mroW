{-# LANGUAGE RecordWildCards #-}

module Game (
  Event(..),
  Game(..),
  createGame,
) where

import Behaviour (Behaviour(..), bScanSplit)
import Camera (Camera, CameraInput(..))
import Common
import Data.Maybe (isJust)
import GameState
import Lens.Micro.Platform
import Player (Player, PlayerInput(..))
import Program (Program(..), RenderInfo(..))

type EventHandler = GameState -> (Program, GameState)
data Event = UpdateGameStateEvent (GameState -> GameState)
           | TickEvent Double DeltaTime
           | RenderEvent

type Game = Behaviour Event Program
createGame :: GameState -> Game
createGame gameState = bScanSplit (flip handleEvent) gameState

handleEvent :: Event -> EventHandler
handleEvent (UpdateGameStateEvent f)   = handleUpdateGameStateEvent f
handleEvent (TickEvent time deltaTime) = handleTickEvent time deltaTime
handleEvent (RenderEvent)              = handleRenderEvent

handleUpdateGameStateEvent :: (GameState -> GameState) -> EventHandler
handleUpdateGameStateEvent f gameState = (NoOp, f gameState)

handleTickEvent :: Double -> DeltaTime -> EventHandler
handleTickEvent time deltaTime gameState = let
  playerInput = PlayerInput {
    _time = time,
    _deltaTime = deltaTime,
    _moveUp = isJust . getVariable varActionUp $ gameState,
    _moveLeft = isJust . getVariable varActionLeft $ gameState,
    _moveDown = isJust . getVariable varActionDown $ gameState,
    _moveRight = isJust . getVariable varActionRight $ gameState
  }
  (_lastPlayerPosition, _player) = getBehaviour (gameState ^. player) playerInput

  cameraInput = CameraInput {
    _time = time,
    _deltaTime = deltaTime,
    _mouseXY = getMousePos gameState,
    _target = _lastPlayerPosition
  }
  _variables = gameState ^. variables
  (_lastCameraPlacement, _camera) = getBehaviour (gameState ^. camera) cameraInput

  in (NoOp, GameState{..})

handleRenderEvent :: EventHandler
handleRenderEvent gameState =
  let (_shadersAreDirty, gameState') = extractDirtyShadersFlag gameState
      _camera = gameState' ^. lastCameraPlacement
      _player = gameState' ^. lastPlayerPosition
      _mousePos = getMousePos gameState'
      _points = getPoints gameState'
  in (Render RenderInfo{..}, gameState')
