module Main where

import Lib
import Platform

main :: IO ()
main = withPlatform "mroW" () $ \(state, platform) -> do
    return (state, False)
