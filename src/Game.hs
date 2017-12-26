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
import Camera (Camera, CameraInput(..), Placement(..), staticCamera, cinematicCamera, freeCamera, getBehaviour)
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
    StaticCamera -> staticCamera (L.zero, fromQuaternion $ L.Quaternion 1 L.zero)
    CinematicCamera -> cinematicCamera
    FreeCamera -> freeCamera

createGameState :: GameState
createGameState = let
    _debugState = createGameDebugState
    _camera = createCamera (_debugState ^. cameraMode)
    in GameState{..}

run :: Double -> Float -> GameState -> (Placement, GameState)
run time deltaSeconds gameState = let
    _debugState = gameState ^. debugState
    cameraInput = CameraInput {
        _time = time,
        _deltaSeconds = deltaSeconds,
        _mouseXY = _debugState ^. mousePos,
        _target = (L.V3 (realToFrac . sin $ time * 0.75) (realToFrac . cos $ time * 0.75) 0) L.^* 0.25
    }
    (cameraPlacement, _camera) = getBehaviour (gameState ^. camera) cameraInput
    in (cameraPlacement, GameState{..})

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
