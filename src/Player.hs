{-# Language RecordWildCards #-}

module Player
  ( PlayerInput(..)
  , Player
  , createPlayer
  ) where

import Behaviour (Behaviour)
import Common
import Control.Arrow (Arrow(..))
import Linear (V3(..))

data PlayerInput = PlayerInput {
  _time :: Time
}

type Player = Behaviour PlayerInput Position

getPlayerPosition :: Double -> V3 Float
getPlayerPosition time =
  let x = (realToFrac . sin $ time * 0.75) * 0.25
      y = (realToFrac . cos $ time * 0.75) * 0.25
   in V3 x y 0

createPlayer :: Position -> Player
createPlayer position = arr $ \PlayerInput{..} ->
  getPlayerPosition _time