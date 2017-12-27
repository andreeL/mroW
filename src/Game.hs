{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Game (
    GameState,
    createGameState,
    run,
    -- debug stuff (where do I put this?)
    setDirtyShadersFlag,
    extractDirtyShadersFlag,
    setMousePos,
    getMousePos,
    useNextCameraMode
    ) where

import Lens.Micro.Platform
import Common
import Camera (Camera, CameraInput(..), createStaticCamera, createCinematicCamera, createFreeCamera, getBehaviour)
import Player (Player, PlayerInput(..), createPlayer)
import Linear as L

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

data GameState = GameState {
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
    CinematicCamera -> createCinematicCamera
    FreeCamera -> createFreeCamera

createGameState :: GameState
createGameState = let
    _debugState = createGameDebugState
    _camera = createCamera (_debugState ^. cameraMode)
    _player = createPlayer L.zero
    in GameState{..}

run :: Double -> Float -> GameState -> (Placement, Position, GameState)
run time deltaSeconds gameState = let
    _debugState = gameState ^. debugState
    playerInput = PlayerInput {
        _time = time
    }
    (playerPosition, _player) = getBehaviour (gameState ^. player) playerInput

    cameraInput = CameraInput {
        _time = time,
        _deltaSeconds = deltaSeconds,
        _mouseXY = _debugState ^. mousePos,
        _target = playerPosition
    }
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

useNextCameraMode :: GameState -> ((), GameState)
useNextCameraMode gameState =
    let newCameraMode = nextCameraMode (gameState ^. debugState ^. cameraMode)
        newCamera = createCamera newCameraMode
     in ((), gameState & camera .~ newCamera & debugState . cameraMode .~ newCameraMode)
