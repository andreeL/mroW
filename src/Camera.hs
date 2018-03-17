{-# Language RecordWildCards #-}
{-# LANGUAGE Arrows #-}

module Camera
  ( CameraInput(..)
  , Camera
  , CameraMode(..)
  , nextCameraMode
  , createCamera
  ) where

import Behaviour (Behaviour(..), bScan)
import Control.Arrow (Arrow(..), returnA)
import Common
import Linear (V2(..), V3(..), M33(..), Quaternion(..), normalize, cross, axisAngle, fromQuaternion, (^+^), (^*), zero)

type Target = V3 Float
type MouseXY = V2 Float

data CameraInput = CameraInput {
  -- this is mostly just placeholder stuff for testing
  _time :: Time,
  _deltaTime :: DeltaTime,
  _mouseXY :: (Double, Double),
  _target :: Target
}

-- a (possibly) stateful camera
type Camera = Behaviour CameraInput Placement

data CameraMode = StaticCamera | CinematicCamera | FreeCamera | LookAtCamera | LookAtRotatingCamera deriving (Bounded, Enum, Eq, Show, Read)
nextCameraMode :: CameraMode -> CameraMode
nextCameraMode cameraMode | cameraMode == maxBound = minBound
                          | otherwise              = succ cameraMode

createCamera :: CameraMode -> Camera
createCamera cameraMode = case cameraMode of
    StaticCamera -> createStaticCamera (V3 0 0 (-1.5), fromQuaternion $ Quaternion 1 zero)
    CinematicCamera -> createCinematicCamera (V3 0 0 (-1.5))
    FreeCamera -> createFreeCamera
    LookAtCamera -> createLookAtCamera
    LookAtRotatingCamera -> createLookAtRotatingCamera

createStaticCamera :: Placement -> Camera
createStaticCamera placement = pure placement

createCinematicCamera :: Position -> Camera
createCinematicCamera position =
  let updatePosition CameraInput{..} = followBehind _deltaTime _target
   in proc cameraInput@CameraInput{..} -> do
    nextPosition <- bScan (flip updatePosition) position -< cameraInput
    returnA -< (nextPosition, lookAt _target nextPosition)

createFreeCamera :: Camera
createFreeCamera = arr $ \CameraInput{..} ->
  let eyePosition = getEyePosition _time
      rotationX = axisAngle (V3 0 1 0) (realToFrac (-(fst _mouseXY)) / 50)
      rotationY = axisAngle (V3 1 0 0) (realToFrac (-(snd _mouseXY)) / 50)
   in (eyePosition, fromQuaternion (rotationY * rotationX))

createLookAtCamera :: Camera
createLookAtCamera = arr $ \CameraInput{..} ->
  let (mouseX, mouseY) = ((realToFrac (-(fst _mouseXY)) / 50), realToFrac (-(snd _mouseXY)) / 50)
      eyePosition = _target + (V3 ((sin mouseX) * (sin mouseY)) (cos mouseY) ((cos mouseX) * (sin mouseY))) ^* 0.5
   in (eyePosition, lookAt _target eyePosition)

createLookAtRotatingCamera :: Camera
createLookAtRotatingCamera = arr $ \CameraInput{..} ->
  let eyePosition = _target + (V3 (sin $ realToFrac _time) 0 (cos $ realToFrac _time)) ^* 0.5
  in (eyePosition, lookAt _target eyePosition)

follow :: DeltaTime -> Position -> Position -> Position
follow deltaTime target origin =
  let toTarget = target - origin
   in origin ^+^ (toTarget ^* (1 - 0.25 ** (getSeconds deltaTime)))

followBehind :: DeltaTime -> Position -> Position -> Position
followBehind deltaTime target position =
  follow deltaTime target $ position ^+^ (V3 0 0 ((getSeconds deltaTime) * (-2)))

lookAt :: Position -> Position -> Rotation
lookAt target origin =
  let vZ = normalize $ target - origin
      vX = normalize $ cross (V3 0 1 0) vZ
      vY = cross vZ vX
   in V3 vX vY vZ

getEyePosition :: Double -> V3 Float
getEyePosition time =
  let x = (realToFrac $ (sin $ time * 0.3) * 1.5)
      y = (realToFrac $ (cos $ time * 0.3) * 1.4)
  in V3 x y (-1)
   