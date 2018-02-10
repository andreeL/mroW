{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Camera (Camera, CameraMode(..), createCamera, nextCameraMode)
import Common
import Data.Map (Map, empty, insert, delete, lookup)
import Data.Maybe (isJust, fromMaybe)
import Lens.Micro.Platform
import Linear (V3(..), Quaternion(..), fromQuaternion, zero)
import Player (Player, createPlayer)
import Prelude hiding (lookup)
import System.Random (StdGen)
import Text.Read (readMaybe)

varActionUp     = "up"
varActionLeft   = "left"
varActionDown   = "down"
varActionRight  = "right"
varPoints       = "points"
varMousePos     = "mousePos"
varCameraMode   = "cameraMode"

type VariableName = String
type VariableValue = String
type Variables = Map VariableName VariableValue

data Model = Cat Int
type GameObject = (Position, Model)

data GameState = GameState {
  _randomGen :: StdGen,
  _variables :: Variables,
  _lastCameraPlacement :: Placement,
  _lastPlayerPosition :: Position,
  _camera :: Camera,
  _player :: Player,
  _gameObjects :: [GameObject]
}
makeLenses ''GameState

createGameState :: StdGen -> GameState
createGameState randomGen = let
  _randomGen = randomGen
  _variables = empty
  _lastCameraPlacement = (V3 0 0 0, fromQuaternion $ Quaternion 1 zero)
  _lastPlayerPosition = zero
  _camera = createCamera CinematicCamera
  _player = createPlayer zero
  _gameObjects = []
  in GameState{..}

setMousePos :: (Double, Double) -> GameState -> ((), GameState)
setMousePos newMousePos = storeVariable varMousePos newMousePos

getMousePos :: GameState -> (Double, Double)
getMousePos = fromMaybe (0, 0) . readVariable varMousePos

addPoints :: Int -> GameState -> ((), GameState)
addPoints points gameState =
  let currentPoints = getPoints gameState
      newPoints = currentPoints + points
  in setVariable varPoints (show newPoints) gameState

getPoints :: GameState -> Int
getPoints gameState = fromMaybe 0 (readMaybe =<< getVariable varPoints gameState)

getCameraMode :: GameState -> CameraMode
getCameraMode = fromMaybe CinematicCamera . readVariable varCameraMode

setCameraMode :: CameraMode -> GameState -> ((), GameState)
setCameraMode cameraMode gameState =
  let gameState' = gameState & camera .~ (createCamera cameraMode)
  in storeVariable varCameraMode cameraMode gameState'

useNextCameraMode :: GameState -> ((), GameState)
useNextCameraMode gameState =
  let newCameraMode = nextCameraMode . getCameraMode $ gameState
  in setCameraMode newCameraMode gameState
  
setVariable :: VariableName -> VariableValue -> GameState -> ((), GameState)
setVariable name value gameState =
  ((), gameState & variables %~ (insert name value))

removeVariable :: VariableName -> GameState -> ((), GameState)
removeVariable name gameState =
  ((), gameState & variables %~ (delete name))

getVariable :: VariableName -> GameState -> Maybe String
getVariable name gameState = lookup name (gameState ^. variables)

storeVariable :: Show a => VariableName -> a -> GameState -> ((), GameState)
storeVariable name value = setVariable name (show value)

readVariable :: Read a => VariableName -> GameState -> Maybe a
readVariable name gameState = readMaybe =<< getVariable name gameState
