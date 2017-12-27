{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Game (
    GameState,
    VariableName,
    VariableValue,

    -- actions
    actionUp,
    actionLeft,
    actionDown,
    actionRight,

    --
    createGameState,
    run,

    -- debug stuff (where do I put this?)
    setDirtyShadersFlag,
    extractDirtyShadersFlag,
    setMousePos,
    getMousePos,
    setVariable,
    removeVariable,
    getVariable,
    useNextCameraMode
    ) where

import Camera (Camera, CameraInput(..), createStaticCamera, createCinematicCamera, createFreeCamera, getBehaviour)
import Common
import Data.Map as M
import Data.Maybe (isJust)
import Lens.Micro.Platform
import Linear as L
import Player (Player, PlayerInput(..), createPlayer)

data CameraMode = StaticCamera | CinematicCamera | FreeCamera deriving (Bounded, Enum, Eq)
nextCameraMode :: CameraMode -> CameraMode
nextCameraMode cameraMode | cameraMode == maxBound = minBound
                          | otherwise              = succ cameraMode

data GameDebugState = GameDebugState {
    _dirtyShadersFlag :: Bool,
    _mousePos :: (Double, Double),
    _cameraMode :: CameraMode
}
makeLenses ''GameDebugState

actionUp = "up" :: String
actionLeft = "left" :: String
actionDown = "down" :: String
actionRight = "right" :: String

type VariableName = String
type VariableValue = String
type Variables = M.Map VariableName VariableValue

data GameState = GameState {
    _variables :: Variables,
    _camera :: Camera,
    _player :: Player,
    _debugState :: GameDebugState
}
makeLenses ''GameState

createGameDebugState :: GameDebugState
createGameDebugState = let
    _dirtyShadersFlag = False
    _mousePos = (0, 0)
    _cameraMode = CinematicCamera
    in GameDebugState{..}

createCamera :: CameraMode -> Camera
createCamera cameraMode = case cameraMode of
    StaticCamera -> createStaticCamera (L.V3 0 0 (-1.5), fromQuaternion $ L.Quaternion 1 L.zero)
    CinematicCamera -> createCinematicCamera (L.V3 0 0 (-1.5))
    FreeCamera -> createFreeCamera

createGameState :: GameState
createGameState = let
    _debugState = createGameDebugState
    _variables = M.empty
    _camera = createCamera (_debugState ^. cameraMode)
    _player = createPlayer L.zero
    in GameState{..}

run :: Double -> Float -> GameState -> (Placement, Position, GameState)
run time deltaSeconds gameState = let
    _debugState = gameState ^. debugState
    playerInput = PlayerInput {
        _time = time,
        _deltaSeconds = deltaSeconds,
        _moveUp = isJust . getVariable actionUp $ gameState,
        _moveLeft = isJust . getVariable actionLeft $ gameState,
        _moveDown = isJust . getVariable actionDown $ gameState,
        _moveRight = isJust . getVariable actionRight $ gameState
    }
    (playerPosition, _player) = getBehaviour (gameState ^. player) playerInput

    cameraInput = CameraInput {
        _time = time,
        _deltaSeconds = deltaSeconds,
        _mouseXY = _debugState ^. mousePos,
        _target = playerPosition
    }
    _variables = gameState ^. variables
    (cameraPlacement, _camera) = getBehaviour (gameState ^. camera) cameraInput
    in (cameraPlacement, playerPosition, GameState{..})

setDirtyShadersFlag :: GameState -> ((), GameState)
setDirtyShadersFlag gameState = ((), (debugState.dirtyShadersFlag.~ True $ gameState))

extractDirtyShadersFlag :: GameState -> (Bool, GameState)
extractDirtyShadersFlag gameState = let currentValue = view (debugState.dirtyShadersFlag) gameState
    in (currentValue, (debugState.dirtyShadersFlag.~ False $ gameState))

setMousePos :: (Double, Double) -> GameState -> ((), GameState)
setMousePos newMousePos gameState = ((), (debugState.mousePos.~ newMousePos $ gameState))

getMousePos :: GameState -> (Double, Double)
getMousePos = view (debugState.mousePos)

setVariable :: VariableName -> VariableValue -> GameState -> ((), GameState)
setVariable variableName variableValue gameState =
    ((), gameState & variables %~ (M.insert variableName variableValue))

removeVariable :: VariableName -> GameState -> ((), GameState)
removeVariable variableName gameState =
    ((), gameState & variables %~ (M.delete variableName))

getVariable :: VariableName -> GameState -> Maybe String
getVariable variableName gameState = M.lookup variableName (gameState ^. variables)

useNextCameraMode :: GameState -> ((), GameState)
useNextCameraMode gameState =
    let newCameraMode = nextCameraMode (gameState ^. debugState ^. cameraMode)
        newCamera = createCamera newCameraMode
     in ((), gameState & camera .~ newCamera & debugState . cameraMode .~ newCameraMode)
