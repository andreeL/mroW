{-# LANGUAGE RecordWildCards #-}

module Game (
  createGame
) where

import Behaviour (Behaviour(..), bScanSplit)
import Camera (Camera, CameraInput(..))
import Common
import Data.Maybe (isJust, fromMaybe)
import Data.List (partition)
import GameState
import qualified Graphics.UI.GLFW as GLFW
import Lens.Micro.Platform
import Linear (V3(..), distance)
import Player (Player, PlayerInput(..))
import Program (EventHandler, Event(..), SceneObject(..), SceneState(..), GUIState(..), Sound(..), Command(..), points)
import System.Random (StdGen, randomR)

type ProgramBuilder = GameState -> ([Command], GameState)

flySpeed = 3 :: Float

createGame :: GameState -> EventHandler
createGame = bScanSplit (flip handleEvent)

handleEvent :: Event -> ProgramBuilder
handleEvent (KeyEvent key scancode action mods) = handleKeyEvent key scancode action mods
handleEvent (MouseEvent x y)                    = handleMouseEvent (x, y)
handleEvent (TickEvent time deltaTime)          = handleTickEvent time deltaTime
handleEvent (UpdateRenderStates)                = handleUpdateRenderStatesEvent


handleKeyEvent :: GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> ProgramBuilder
handleKeyEvent key scancode action mods state =
  let setOrRemove variableName variableValue doSet = snd . if (doSet == True)
        then setVariable variableName variableValue
        else removeVariable variableName
  
      keyDown = action /= GLFW.KeyState'Released
  
      state' = fromMaybe state $ case key of
        GLFW.Key'W -> Just $ setOrRemove varActionUp "" keyDown state
        GLFW.Key'A -> Just $ setOrRemove varActionLeft "" keyDown state
        GLFW.Key'S -> Just $ setOrRemove varActionDown "" keyDown state
        GLFW.Key'D -> Just $ setOrRemove varActionRight "" keyDown state
        _ -> Nothing

      (program, state'') = fromMaybe ([], state') $ if action == GLFW.KeyState'Pressed then
        case key of
          GLFW.Key'F2 -> Just ([], snd . useNextCameraMode $ state')
          _ -> Nothing
        else Nothing
  
  in (program, state'')

handleMouseEvent :: (Double, Double) -> ProgramBuilder
handleMouseEvent mousePos state = ([], snd . setMousePos mousePos $ state)

handleUpdateEvent :: (GameState -> GameState) -> ProgramBuilder
handleUpdateEvent f state = ([], f state)

handleTickEvent :: Double -> DeltaTime -> ProgramBuilder
handleTickEvent time deltaTime state@GameState{..} = let
  isSet var = isJust . getVariable var $ state
  
  (playerPosition', player') = getBehaviour _player PlayerInput {
    _time = time,
    _deltaTime = deltaTime,
    _moveUp = isJust . getVariable varActionUp $ state,
    _moveLeft = isJust . getVariable varActionLeft $ state,
    _moveDown = isJust . getVariable varActionDown $ state,
    _moveRight = isJust . getVariable varActionRight $ state
  }

  (cameraPlacement', camera') = getBehaviour _camera CameraInput {
    _time = time,
    _deltaTime = deltaTime,
    _mouseXY = getMousePos state,
    _target = playerPosition'
  }
  
  (updatedGameObjects, randomGen') = updateGameObjects (getSeconds deltaTime) _gameObjects _randomGen
  (gameObjects', takenGameObjects) = tryTakeGameObjects playerPosition' updatedGameObjects
  noOfObjectsTaken = length takenGameObjects
  playerPoints' = _playerPoints + noOfObjectsTaken

  in (
    if (noOfObjectsTaken > 0) then [PlaySound $ Piano playerPoints'] else [],
    GameState {
      _randomGen = randomGen',
      _variables = _variables,
      _lastCameraPlacement = cameraPlacement',
      _lastPlayerPosition = playerPosition',
      _camera = camera',
      _player = player',
      _playerPoints = playerPoints',
      _playerEnergy = max 0 . min 1 $ (fromIntegral noOfObjectsTaken) + _playerEnergy - (getSeconds deltaTime) * 0.1,
      _gameObjects = gameObjects'
    }
  )

handleUpdateRenderStatesEvent :: ProgramBuilder
handleUpdateRenderStatesEvent state@GameState{..} =
  let sceneState = SceneState { _camera = _lastCameraPlacement,
                                _player = _lastPlayerPosition,
                                _objects = fmap toSceneObject _gameObjects
                              }
      guiState = GUIState { _points = _playerPoints,
                            _energy = _playerEnergy,
                            _currentMenuOption = Nothing
                          }
      commands = [ UpdateScene $ const sceneState,
                   UpdateGUI $ const guiState
                 ]
  in (commands, state)

toSceneObject :: GameObject -> SceneObject
toSceneObject (position, Cat skin) = SceneObject {
  _objectPosition = position,
  _objectType = skin
}

updateGameObjects :: Float -> [GameObject] -> StdGen -> ([GameObject], StdGen)
updateGameObjects time gameObjects randomGen =
  let moveForward (V3 x y z, model) = (V3 x y (z + time * flySpeed), model)
      movedObjects = filter (\(V3 _ _ z, _) -> z < 20) . fmap moveForward $ gameObjects
      (addedObjects, randomGen') =
        let minZ = -5
            minDistance = 4
            (x, randomGen') = randomR (-1, 1) randomGen
            (y, randomGen'') = randomR (-1, 1) randomGen'
            (z, randomGen''') = randomR (minZ - minDistance - 10, minZ - minDistance) randomGen''
            newObjectNeeded [] = True
            newObjectNeeded xs = (>minZ) . minimum . fmap (\(V3 _ _ z, _) -> z) $ xs
        in if newObjectNeeded movedObjects then ([(V3 x y z, Cat 1)], randomGen''') else ([], randomGen)
   in (addedObjects ++ movedObjects, randomGen')

tryTakeGameObjects :: Position -> [GameObject] -> ([GameObject], [GameObject])
tryTakeGameObjects playerPosition gameObjects = partition (\(objectPosition, _) -> (playerPosition `distance` objectPosition) > 0.25) gameObjects