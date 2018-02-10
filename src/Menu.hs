{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Lens.Micro.Platform
import Program (EventHandler, Event(..), GUIState(..), Command(..), currentMenuOption)
import System.Random (mkStdGen)
import qualified Graphics.UI.GLFW as GLFW

data MenuOption = StartGame
                | Exit

mainMenuOptions = [StartGame, Exit] :: [MenuOption]
type Menu = (Int, [MenuOption])

-- For now, for simplicity, we say that the menu always exist, and is the owner of the game
-- This might change in the future though
data MenuState = MenuState {
  _game :: Maybe EventHandler,
  _menu :: Maybe Menu
}

type ProgramBuilder = MenuState -> ([Command], MenuState)

createMenuState :: MenuState
createMenuState = MenuState {
  _game = Nothing,
  _menu = Just (0, mainMenuOptions)
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
        GLFW.Key'Up -> Just ([], moveMenuOption (-1) menuState)
        GLFW.Key'Down -> Just ([], moveMenuOption 1 menuState)
        GLFW.Key'Escape -> Just ([], toggleMenu menuState)
        GLFW.Key'Enter -> Just (handleEnterPressedEvent menuState)
        _ -> Nothing
      else Nothing
  , Just $ withGame (\game -> getBehaviour game (KeyEvent key scancode action mods)) menuState
  ]

toggleMenu :: MenuState -> MenuState
toggleMenu MenuState{..} = MenuState{
    _game = _game,
    _menu = case _menu of
      Just menu -> Nothing
      Nothing -> Just (0, mainMenuOptions)
  }

handleEnterPressedEvent :: ProgramBuilder
handleEnterPressedEvent menuState@MenuState{..} = fromMaybe ([], menuState) $ case _menu of
    Just menu -> case ((snd menu) ^? ix (fst menu)) of
      Just StartGame -> Just ([], startGame menuState)
      Just Exit -> Just ([Terminate], menuState)
      _ -> Nothing
    Nothing -> Nothing

handleMouseEvent :: Double -> Double -> ProgramBuilder
handleMouseEvent x y = withGame (\game -> getBehaviour game (MouseEvent x y))

handleTickEvent :: Double -> DeltaTime -> ProgramBuilder
handleTickEvent time deltaTime = withGame (\game -> getBehaviour game (TickEvent time deltaTime))

handleUpdateRenderStatesEvent :: ProgramBuilder
handleUpdateRenderStatesEvent menuState =
  let (commands, menuState') = withGame (\game -> getBehaviour game UpdateRenderStates) menuState
   in (commands ++ [UpdateGUI $ \guiState -> guiState & currentMenuOption .~ (fmap fst . _menu $ menuState)], menuState') -- TODO...

moveMenuOption :: Int -> MenuState -> MenuState
moveMenuOption steps MenuState{..} = MenuState{
    _game = _game,
    _menu = fmap move _menu
  }
  where move (currentIndex, menuOptions) = ((currentIndex + steps) `mod` (length menuOptions), menuOptions)

startGame :: MenuState -> MenuState
startGame MenuState{..} = MenuState{
    _game = Just . fromMaybe (createGame $ createGameState $ mkStdGen 0) $ _game, -- TODO we need different seeds
    _menu = Nothing
  }

withGame :: (EventHandler -> ([Command], EventHandler)) -> MenuState -> ([Command], MenuState)
withGame f menuState@MenuState{..} =
  if (isJust _menu)
  then ([], menuState)
  else case _game of
    Just game -> let (commands, game') = f game in (commands, MenuState{_game = Just game', _menu = _menu})
    Nothing -> ([], menuState)
