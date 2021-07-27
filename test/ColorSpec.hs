{-# LANGUAGE BlockArguments #-}

module ColorSpec where

import Drawing.Color (
    Color (colorB, colorG, colorR),
    addC,
    mkColor,
    multC,
    multScalarC,
    subC,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Color" do
    it "Should have the correct r,g,b values" do
        let c = mkColor (-0.5) 0.4 1.7
        colorR c `shouldBe` (-0.5)
        colorG c `shouldBe` 0.4
        colorB c `shouldBe` 1.7

    describe "Operations" do
        it "Should add colors" do
            let c1 = mkColor 0.9 0.6 0.75
                c2 = mkColor 0.7 0.1 0.25
                expected = mkColor 1.6 0.7 1
                actual = c1 `addC` c2
            actual `shouldBe` expected
        it "Should subtract colors" do
            let c1 = mkColor 0.9 0.6 0.75
                c2 = mkColor 0.7 0.1 0.25
                expected = mkColor 0.2 0.5 0.5
                actual = c1 `subC` c2
            actual `shouldBe` expected
        it "Should multiply a color by scalar" do
            let c = mkColor 0.2 0.3 0.4
                expected = mkColor 0.4 0.6 0.8
                actual = c `multScalarC` 2
            actual `shouldBe` expected
        it "Should multiply colors" do
            let c1 = mkColor 1 0.2 0.4
                c2 = mkColor 0.9 1 0.1
                expected = mkColor 0.9 0.2 0.04
                actual = c1 `multC` c2
            actual `shouldBe` expected