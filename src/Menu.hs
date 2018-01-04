module Menu (
  createMenu
) where

import Behaviour (Behaviour(..), bScanSplit)
import Common
import Program (Program(..))

data MenuState = MenuState {
  _void :: ()
}

data Event = UpdateEvent (MenuState -> MenuState)
           | TickEvent Double DeltaTime
           | RenderEvent

type Menu = Behaviour Event Program
type ProgramBuilder = MenuState -> (Program, MenuState)

createMenu :: MenuState -> Menu
createMenu state = bScanSplit (flip handleEvent) state

handleEvent :: Event -> ProgramBuilder
handleEvent (UpdateEvent f)            = handleUpdateEvent f
handleEvent (TickEvent time deltaTime) = handleTickEvent time deltaTime
handleEvent (RenderEvent)              = handleRenderEvent

handleUpdateEvent :: (MenuState -> MenuState) -> ProgramBuilder
handleUpdateEvent f state = (NoOp, f state)

handleTickEvent :: Double -> DeltaTime -> ProgramBuilder
handleTickEvent time deltaTime state = (NoOp, state)

handleRenderEvent :: ProgramBuilder
handleRenderEvent state = (NoOp, state)
