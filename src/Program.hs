{-# LANGUAGE TemplateHaskell #-}

module Program where

import Behaviour (Behaviour)
import Common (Placement, Position, DeltaTime)
import Graphics.UI.GLFW (Key, KeyState, ModifierKeys)
import qualified Lens.Micro.Platform as Lens
import Linear (zero, identity)

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
Lens.makeLenses ''SceneState

createSceneState = SceneState {
  _camera = (zero, identity),
  _player = zero
}

data GUIState = GUIState {
  --_mousePos :: (Double, Double), -- TODO: we don't really use this for anything ATM so we could remove it
  _points :: Int,
  _currentMenuOption :: Maybe Int
}
Lens.makeLenses ''GUIState

createGUIState = GUIState {
  _points = 0,
  _currentMenuOption = Nothing
}

data Sound = Piano Int -- piano is just for testing

data Command = Terminate
             | MarkShadersAsDirty
             | UpdateScene (SceneState -> SceneState)
             | UpdateGUI (GUIState -> GUIState)
             | PlaySound Sound
             | Log String

type EventHandler = Behaviour Event [Command]
             