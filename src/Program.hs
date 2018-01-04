module Program where

import Common (Placement, Position)

data RenderInfo = RenderInfo {
  _shadersAreDirty :: Bool,
  _camera :: Placement,
  _player :: Position,
  _mousePos :: (Double, Double),
  _points :: Int
}

data Program = NoOp
             | Render RenderInfo
