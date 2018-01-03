{-# Language RecordWildCards #-}
{-# LANGUAGE Arrows #-}

module Camera
  ( Placement
  , CameraInput(..)
  , Camera
  , getBehaviour
  , createStaticCamera
  , createCinematicCamera
  , createFreeCamera
  ) where

import Behaviour (Behaviour(..), bScan)
import Control.Arrow (Arrow(..), returnA)
import Common
import Linear (V2(..), V3(..), M33(..), normalize, cross, axisAngle, fromQuaternion, (^+^), (^*))

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

getEyePosition :: Double -> V3 Float
getEyePosition time =
  let x = (realToFrac $ (sin $ time * 0.3) * 1.5)
      y = (realToFrac $ (cos $ time * 0.3) * 1.4)
   in V3 x y (-1)

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
