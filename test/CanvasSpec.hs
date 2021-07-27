{-# LANGUAGE BlockArguments #-}

module CanvasSpec where

import Drawing.Canvas (
    Canvas (canvasHeight, canvasWidth),
    mkCanvas,
    pixelAt,
    toList,
    writePixel,
 )
import Drawing.Color (mkColor)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Canvas" do
    it "Should create a canvas" do
        let black = mkColor 0 0 0
            c = mkCanvas 10 20 black
        canvasWidth c `shouldBe` 10
        canvasHeight c `shouldBe` 20
        length (toList c) `shouldBe` 200
        all (\(_, c) -> c == black) (toList c) `shouldBe` True
    it "Should write pixels to a canvas" do
        let black = mkColor 0 0 0
            c = mkCanvas 10 20 black
            red = mkColor 1 0 0
            newCanvas = writePixel (2, 3) red c
        (newCanvas >>= pixelAt (2, 3)) `shouldBe` Just red