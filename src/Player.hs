{-# Language RecordWildCards #-}
{-# LANGUAGE Arrows #-}

module Player
  ( PlayerInput(..)
  , Player
  , createPlayer
  ) where

import Behaviour (Behaviour, bScan)
import Common
import Control.Arrow (Arrow(..), (>>>), returnA)
import Linear (V3(..), (^+^), (^-^), (^*), dot, norm, signorm, normalize, zero)

data PlayerInput = PlayerInput {
  _time :: Time,
  _deltaTime :: DeltaTime,
  _moveUp :: Bool,
  _moveLeft :: Bool,
  _moveDown :: Bool,
  _moveRight :: Bool
}

data Spring = Spring {
  _springPosition :: Position,
  _springRestingLength :: Float,
  _springConstant :: Float,
  _springDampening :: Float
}

type Player = Behaviour PlayerInput Position

wallSpring =
  let _springPosition = zero
      _springRestingLength = 1.5
      _springConstant = 100
      _springDampening = 2
   in Spring{..}

createPlayer position = proc playerInput@PlayerInput{..} -> do
  let acceleration = thrusterAcceleration playerInput
  (_, position') <- advancePlayerState (zero, position) -< (_deltaTime, acceleration)
  returnA -< position'
  
thrusterAcceleration :: PlayerInput -> Acceleration
thrusterAcceleration PlayerInput{..} =
  let x = (if _moveRight then 1 else 0) + (if _moveLeft then (-1) else 0)
      y = (if _moveUp then 1 else 0) + (if _moveDown then (-1) else 0)
      strength = 20.0
  in (normalize $ V3 x y 0) ^* strength

advancePlayerState :: (Velocity, Position) -> Behaviour (DeltaTime, Acceleration) (Velocity, Position)
advancePlayerState = bScan $ \state (deltaSeconds, staticAcceleration) ->
  let acceleration = staticAcceleration + (wallSpringAcceleration state)
   in integrate deltaSeconds (const acceleration) state

wallSpringAcceleration :: (Velocity, Position) -> Acceleration
wallSpringAcceleration (velocity, position)
  | position `dot` acceleration > 0 = zero -- we never push the player outwards
  | otherwise                       = acceleration
  where force = springForce wallSpring (velocity, position)
        acceleration = force -- we just treat it as the acceleration

springForce :: Spring -> (Velocity, Position) -> Acceleration
springForce Spring{..} (velocity, position)
  | abs springExtension < 0.0001 = zero
  | otherwise  = springNormal ^* force
    where diff = position ^-^ _springPosition
          springExtension = norm diff
          springNormal = signorm diff
          -- Hooke's law with dampening
          x = springExtension - _springRestingLength
          v = springNormal `dot` velocity
          force = -(_springConstant * x) - (_springDampening * v)

integrate :: DeltaTime -> ((Velocity, Position) -> Acceleration) -> (Velocity, Position) -> (Velocity, Position)
integrate deltaTime getAcceleration (velocity, position) =
  let deltaSeconds = getSeconds deltaTime
      integratedHalfAcceleration = (getAcceleration (velocity, position)) ^* (0.5 * deltaSeconds)
      friction = 0.1 ** (deltaSeconds * 0.5)
      intermediateVelocity = (velocity ^* friction) ^+^ integratedHalfAcceleration
      velocity' = (intermediateVelocity ^* friction) ^+^ integratedHalfAcceleration
      position' = position ^+^ (intermediateVelocity ^* deltaSeconds)
    in (velocity', position')
