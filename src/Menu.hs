module Menu (
  createMenuState,
  createMenu
) where

import Behaviour (Behaviour(..), getBehaviour, bScanSplit)
import Common
import Data.Maybe (isJust, fromMaybe)
import Game (createGame)
import GameState (createGameState)
import Program (EventHandler, Event(..), Program(..))
import qualified Graphics.UI.GLFW as GLFW

-- For now, for simplicity, we say that the menu always exist, and is the owner of the game
-- This might change in the future though
data MenuState = MenuState {
  _game :: Maybe EventHandler
}

type ProgramBuilder = MenuState -> (Program, MenuState)

createMenuState :: MenuState
createMenuState = MenuState {
  _game = Nothing
}

createMenu :: MenuState -> EventHandler
createMenu state = bScanSplit (flip handleEvent) state

handleEvent :: Event -> ProgramBuilder
handleEvent (KeyEvent key scancode action mods) = handleKeyEvent key scancode action mods
handleEvent (MouseEvent x y)                    = handleMouseEvent x y
handleEvent (TickEvent time deltaTime)          = handleTickEvent time deltaTime
handleEvent (RenderEvent)                       = handleRenderEvent

-- TODO: these are just temporay implementations and will be replaced completely
handleKeyEvent :: GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> ProgramBuilder
handleKeyEvent key scancode action mods = withGame (\game -> getBehaviour game (KeyEvent key scancode action mods))

handleMouseEvent :: Double -> Double -> ProgramBuilder
handleMouseEvent x y = withGame (\game -> getBehaviour game (MouseEvent x y))

handleTickEvent :: Double -> DeltaTime -> ProgramBuilder
handleTickEvent time deltaTime = withGame (\game -> getBehaviour game (TickEvent time deltaTime))

handleRenderEvent :: ProgramBuilder
handleRenderEvent = withGame (\game -> getBehaviour game RenderEvent)

-- TODO: just for testing
withGame :: (EventHandler -> (a, EventHandler)) -> MenuState -> (a, MenuState)
withGame f state =
  let game = fromMaybe (createGame createGameState) (_game state)
      (a, game') = f game
  in (a, MenuState{_game = Just game'})
  