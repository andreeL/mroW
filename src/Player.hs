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
import Linear (V3(..), (^+^), (^*), normalize, zero)

data PlayerInput = PlayerInput {
  _time :: Time,
  _deltaSeconds :: DeltaTime,
  _moveUp :: Bool,
  _moveLeft :: Bool,
  _moveDown :: Bool,
  _moveRight :: Bool
}

type Player = Behaviour PlayerInput Position

createPlayer position = proc playerInput@PlayerInput{..} -> do
  let acceleration = thrusterAcceleration playerInput
  (_, position') <- advancePlayerState (zero, position) -< (_deltaSeconds, acceleration)
  returnA -< position'
  
thrusterAcceleration :: PlayerInput -> Acceleration
thrusterAcceleration PlayerInput{..} =
  let x = (if _moveRight then 1 else 0) + (if _moveLeft then (-1) else 0)
      y = (if _moveUp then 1 else 0) + (if _moveDown then (-1) else 0)
      strength = 20.0
  in (normalize $ V3 x y 0) ^* strength

advancePlayerState :: (Velocity, Position) -> Behaviour (DeltaTime, Acceleration) (Velocity, Position)
advancePlayerState = bScan $ \state (deltaSeconds, thrusterAcceleration) ->
  integrate deltaSeconds (const thrusterAcceleration) state

integrate :: DeltaTime -> ((Velocity, Position) -> Acceleration) -> (Velocity, Position) -> (Velocity, Position)
integrate deltaSeconds getAcceleration (velocity, position) =
  let integratedHalfAcceleration = (getAcceleration (velocity, position)) ^* (0.5 * deltaSeconds)
      friction = 0.1 ** deltaSeconds
      intermediateVelocity = (velocity ^* friction) ^+^ integratedHalfAcceleration
      velocity' = intermediateVelocity ^+^ integratedHalfAcceleration
      position' = position ^+^ (intermediateVelocity ^* deltaSeconds)
    in (velocity', position')
