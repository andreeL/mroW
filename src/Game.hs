{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Game (
    GameState,
    createGameState,

    -- debug stuff (where do I put this?)
    setDirtyShadersFlag,
    extractDirtyShadersFlag,
    setMousePos,
    getMousePos
    ) where

import Lens.Micro.Platform

data GameDebugState = GameDebugState {
    _dirtyShadersFlag :: Bool,
    _mousePos :: (Double, Double)
} deriving (Show, Eq)
makeLenses ''GameDebugState

data GameState = GameState {
    _debugState :: GameDebugState
} deriving (Show, Eq)
makeLenses ''GameState

createGameDebugState :: GameDebugState
createGameDebugState = let
    _dirtyShadersFlag = False
    _mousePos = (0, 0)
    in GameDebugState{..}

createGameState :: GameState
createGameState = GameState createGameDebugState

setDirtyShadersFlag :: GameState -> ((), GameState)
setDirtyShadersFlag gameState = ((), (debugState.dirtyShadersFlag.~ True $ gameState))

extractDirtyShadersFlag :: GameState -> (Bool, GameState)
extractDirtyShadersFlag gameState = let currentValue = view (debugState.dirtyShadersFlag) gameState
    in (currentValue, (debugState.dirtyShadersFlag.~ False $ gameState))

setMousePos :: (Double, Double) -> GameState -> ((), GameState)
setMousePos newMousePos gameState = ((), (debugState.mousePos.~ newMousePos $ gameState))

getMousePos :: GameState -> (Double, Double)
getMousePos = view (debugState.mousePos)