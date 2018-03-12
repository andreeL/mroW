{-# LANGUAGE TemplateHaskell #-}

module Program where

import Behaviour (Behaviour)
import Common (Placement, Position, DeltaTime)
import Graphics.UI.GLFW (Key, KeyState, ModifierKeys)
import qualified Lens.Micro.Platform as Lens
import Linear (V3(..), zero, identity)

-- events
data Event = KeyEvent Key Int KeyState ModifierKeys
           | MouseEvent Double Double
           | TickEvent Double DeltaTime
           | UpdateRenderStates

-- possible outcomes (programs)
data SceneObject = SceneObject {
  _objectPosition :: V3 Float,
  _objectType :: Int
}

data SceneState = SceneState {
  _camera :: Placement,
  _player :: Position,
  _objects :: [SceneObject]
}

Lens.makeLenses ''SceneState

createSceneState = SceneState {
  _camera = (zero, identity),
  _player = zero,
  _objects = []
}

data GUIState = GUIState {
  --_mousePos :: (Double, Double), -- TODO: we don't really use this for anything ATM so we could remove it
  _points :: Int,
  _energy :: Float,
  _showGameOver :: Bool,
  _currentMenuOption :: Maybe Int
}
Lens.makeLenses ''GUIState

createGUIState = GUIState {
  _points = -1,
  _energy = 0,
  _showGameOver = False,
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
             