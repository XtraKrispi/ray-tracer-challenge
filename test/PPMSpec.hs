{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module PPMSpec where

import Drawing.Canvas (mkCanvas, writePixel)
import Drawing.Color (mkColor)
import Drawing.Output (
    FileType (PPMType),
    PPM (PPM),
    PPMHeader (PPMHeader),
    convertToPPM,
    getOutput,
    getRawFile,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "PPM format" do
    it "Should construct the header correctly" do
        let c = mkCanvas 5 3 (mkColor 0 0 0)
            PPM (PPMHeader ident w h maxC) _ = convertToPPM c
        ident `shouldBe` "P3"
        w `shouldBe` 5
        h `shouldBe` 3
        maxC `shouldBe` 255
    it "Should have the correct lines for the header" do
        let output = getOutput PPMType $ mkCanvas 5 3 (mkColor 0 0 0)
            raw = getRawFile output
        take 3 raw `shouldBe` ["P3", "5 3", "255"]
    it "Should construct proper pixel data" do
        let c = mkCanvas 5 3 (mkColor 0 0 0)
            c1 = mkColor 1.5 0 0
            c2 = mkColor 0 0.5 0
            c3 = mkColor (-0.5) 0 1
            Just newC = writePixel (0, 0) c1 c >>= writePixel (2, 1) c2 >>= writePixel (4, 2) c3
            PPM _ p = convertToPPM newC
            [line1, line2, line3] = take 3 p
        head line1 `shouldBe` (255, 0, 0)
        line2 !! 2 `shouldBe` (0, 128, 0)
        line3 !! 4 `shouldBe` (0, 0, 255)
    it "Should split long lines in PPM files" do
        let raw = getRawFile $ getOutput PPMType $ mkCanvas 10 2 (mkColor 1 0.8 0.6)
            [line1, line2, line3, line4] = take 4 . drop 3 $ raw

        line1 `shouldBe` "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
        line2 `shouldBe` "153 255 204 153 255 204 153 255 204 153 255 204 153"
        line3 `shouldBe` "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
        line4 `shouldBe` "153 255 204 153 255 204 153 255 204 153 255 204 153"
    it "Should terminate raw files with a newline" do
        let raw = getRawFile $ getOutput PPMType $ mkCanvas 5 3 (mkColor 0 0 0)
        last raw `shouldBe` "\n"