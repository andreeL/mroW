{-# LANGUAGE RecordWildCards #-}

module Menu (
  createMenuState,
  createMenu
) where

import Behaviour (Behaviour(..), getBehaviour, bScanSplit)
import Common
import Data.Foldable (asum)
import Data.Maybe (isJust, fromMaybe)
import Game (createGame)
import GameState (createGameState)
import Program (EventHandler, Event(..), GUIState(..), Command(..))
import qualified Graphics.UI.GLFW as GLFW

data MenuOption = StartGame
                | Exit

mainMenuOptions = [StartGame, Exit] :: [MenuOption]
type Menu = (Int, [MenuOption])

-- For now, for simplicity, we say that the menu always exist, and is the owner of the game
-- This might change in the future though
data MenuState = MenuState {
  _game :: Maybe EventHandler,
  _menu :: Menu
}

type ProgramBuilder = MenuState -> ([Command], MenuState)

createMenuState :: MenuState
createMenuState = MenuState {
  _game = Nothing,
  _menu = (0, mainMenuOptions)
}

createMenu :: MenuState -> EventHandler
createMenu = bScanSplit (flip handleEvent)

handleEvent :: Event -> ProgramBuilder
handleEvent (KeyEvent key scancode action mods) = handleKeyEvent key scancode action mods
handleEvent (MouseEvent x y)                    = handleMouseEvent x y
handleEvent (TickEvent time deltaTime)          = handleTickEvent time deltaTime
handleEvent (UpdateRenderStates)                = handleUpdateRenderStatesEvent

-- TODO: these are just temporay implementations and will be replaced completely
handleKeyEvent :: GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> ProgramBuilder
handleKeyEvent key scancode action mods menuState = fromMaybe ([], menuState) $ asum
  [ if action == GLFW.KeyState'Pressed then
      case key of
        GLFW.Key'F1 -> Just ([MarkShadersAsDirty], menuState)
        GLFW.Key'Up -> let menuState' = moveMenuOption (-1) menuState in Just ([Log (show.fst._menu $ menuState')], menuState')
        GLFW.Key'Down -> let menuState' = moveMenuOption 1 menuState in Just ([Log (show.fst._menu $ menuState')], menuState')
        _ -> Nothing
      else Nothing
  , Just $ withGame (\game -> getBehaviour game (KeyEvent key scancode action mods)) menuState
  ]

handleMouseEvent :: Double -> Double -> ProgramBuilder
handleMouseEvent x y = withGame (\game -> getBehaviour game (MouseEvent x y))

handleTickEvent :: Double -> DeltaTime -> ProgramBuilder
handleTickEvent time deltaTime = withGame (\game -> getBehaviour game (TickEvent time deltaTime))

handleUpdateRenderStatesEvent :: ProgramBuilder
handleUpdateRenderStatesEvent menuState =
  let (commands, menuState') = withGame (\game -> getBehaviour game UpdateRenderStates) menuState
   in (commands ++ [], menuState') -- TODO...

moveMenuOption :: Int -> MenuState -> MenuState
moveMenuOption steps (MenuState game (currentIndex, menuOptions)) = MenuState game ((currentIndex + steps) `mod` (length menuOptions), menuOptions)

-- TODO: just for testing
withGame :: (EventHandler -> (a, EventHandler)) -> MenuState -> (a, MenuState)
withGame f MenuState{..} =
  let game = fromMaybe (createGame createGameState) _game
      (commands, game') = f game
  in (commands, MenuState{_game = Just game', _menu = _menu})
  