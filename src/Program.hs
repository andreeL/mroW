module Program where

import Common (Placement, Position, DeltaTime)
  
data SceneInfo = SceneInfo {
  _shadersAreDirty :: Bool,
  _camera :: Placement,
  _player :: Position,
  _mousePos :: (Double, Double),
  _points :: Int
}

data Program = NoOp
             | RenderScene SceneInfo
