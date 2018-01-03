module Common where

import Linear (V3(..), M33(..))

type Time = Double
newtype DeltaTime = DeltaTime {getSeconds :: Float}
type Acceleration = V3 Float
type Velocity = V3 Float
type Position = V3 Float
type Rotation = M33 Float
type Placement = (Position, Rotation)
