module Program where

import Behaviour (Behaviour)
import Common (Placement, Position, DeltaTime)
import Graphics.UI.GLFW (Key, KeyState, ModifierKeys)

type EventHandler = Behaviour Event Program

-- events
data Event = KeyEvent Key Int KeyState ModifierKeys
           | MouseEvent Double Double
           | TickEvent Double DeltaTime
           | RenderEvent

-- possible outcomes (programs)
data SceneInfo = SceneInfo {
  _shadersAreDirty :: Bool,
  _camera :: Placement,
  _player :: Position,
  _mousePos :: (Double, Double),
  _points :: Int
}

data Program = NoOp
             | RenderScene SceneInfo
