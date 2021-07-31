module LightSpec where

import Drawing.Color
import Light
import Test.Hspec
import Tuple

spec :: Spec
spec = describe "Point Lights" do
    it "Should have a position and intensity" do
        let intensity = mkColor 1 1 1
            position = mkPoint 0 0 0
            (PointLight p i) = mkPointLight position intensity

        p `shouldBe` position
        i `shouldBe` intensity