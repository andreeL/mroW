module GameSpec (spec) where

import Test.Hspec
import Game

spec :: Spec
spec =
  describe "createGameState" $ do
    it "constructs something (not undefined)" $
      createGameState `shouldBe` createGameState
