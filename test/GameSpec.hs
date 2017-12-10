module GameSpec (spec) where

import Test.Hspec
import Game

spec :: Spec
spec =
  describe "createGameState" $ do
    it "constructs" $
      createGameState `shouldBe` GameState
