module Common where

import Linear (V3(..), M33(..))

type Time = Double
type DeltaTime = Float
type Position = V3 Float
type Rotation = M33 Float
type Placement = (Position, Rotation)
