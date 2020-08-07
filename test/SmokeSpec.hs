module SmokeSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Smoke test" $ do
        it "ShouldBe true" $
            True `shouldBe` True
