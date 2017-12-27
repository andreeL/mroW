{-# Language RecordWildCards #-}

module Player
  ( PlayerInput(..)
  , Player
  , createPlayer
  ) where

import Behaviour (Behaviour, bScan)
import Common
import Control.Arrow (Arrow(..))
import Linear (V3(..), (^+^), (^*), normalize)

data PlayerInput = PlayerInput {
  _time :: Time,
  _moveUp :: Bool,
  _moveLeft :: Bool,
  _moveDown :: Bool,
  _moveRight :: Bool
}

type Player = Behaviour PlayerInput Position

createPlayer :: Position -> Player
createPlayer position = bScan updatePosition position
  where updatePosition currentPosition PlayerInput{..} =
          let x = (if _moveRight then 1 else 0) + (if _moveLeft then (-1) else 0)
              y = (if _moveUp then 1 else 0) + (if _moveDown then (-1) else 0)
              speed = 0.02
           in (normalize $ V3 x y 0) ^* speed ^+^ currentPosition