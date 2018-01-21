module Program where

import Behaviour (Behaviour)
import Common (Placement, Position, DeltaTime)
import Graphics.UI.GLFW (Key, KeyState, ModifierKeys)
import Linear (zero, identity)

type EventHandler = Behaviour Event [Command]

-- events
data Event = KeyEvent Key Int KeyState ModifierKeys
           | MouseEvent Double Double
           | TickEvent Double DeltaTime
           | UpdateRenderStates

-- possible outcomes (programs)
data SceneState = SceneState {
  _camera :: Placement,
  _player :: Position
}

createSceneState = SceneState {
  _camera = (zero, identity),
  _player = zero
}

data GUIState = GUIState {
  --_mousePos :: (Double, Double), -- TODO: we don't really use this for anything ATM so we could remove it
  _points :: Int
}

createGUIState = GUIState {
  _points = 0
}

data Command = ReloadShaders
             | UpdateScene (SceneState -> SceneState)
             | UpdateGUI (GUIState -> GUIState)
             | Log String
