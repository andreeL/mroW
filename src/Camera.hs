{-# Language RecordWildCards #-}

module Camera
  ( Placement
  , CameraInput(..)
  , Camera
  , getBehaviour
  , createStaticCamera
  , createCinematicCamera
  , createFreeCamera
  ) where

import Behaviour (Behaviour(..))
import Control.Arrow (Arrow(..))
import Common
import Linear (V2(..), V3(..), M33(..), normalize, cross, axisAngle, fromQuaternion)

type Target = V3 Float
type MouseXY = V2 Float

data CameraInput = CameraInput {
  -- this is mostly just placeholder stuff for testing
  _time :: Time,
  _deltaSeconds :: DeltaTime,
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

createCinematicCamera :: Camera
createCinematicCamera = arr $ \CameraInput{..} ->
  let eyePosition = getEyePosition _time
      vZ = normalize $ _target - eyePosition
      vX = normalize $ cross vZ (V3 0 1 0)
      vY = cross vX vZ
   in (eyePosition, V3 vX vY vZ)
            
createFreeCamera :: Camera
createFreeCamera = arr $ \CameraInput{..} ->
  let eyePosition = getEyePosition _time
      rotationX = axisAngle (V3 0 1 0) (realToFrac (-(fst _mouseXY)) / 50)
      rotationY = axisAngle (V3 1 0 0) (realToFrac (-(snd _mouseXY)) / 50)
   in (eyePosition, fromQuaternion (rotationY * rotationX))
