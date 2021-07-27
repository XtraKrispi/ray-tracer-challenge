{-# LANGUAGE BlockArguments #-}

module TransformationSpec where

import Matrix (inverse)
import qualified Matrix
import Test.Hspec (Spec, describe, it, shouldBe)
import Transformation (rotationX, rotationY, rotationZ, scaling, translation)
import Tuple (mkPoint, mkVector)

spec :: Spec
spec = describe "Transformations" do
    describe "Translation" do
        it "Should multiply by translation matrix" do
            let transform = translation 5 (-3) 2
                p = mkPoint (-3) 4 5
            Matrix.multT transform p `shouldBe` mkPoint 2 1 7

        it "Should correctly multiply by the inverse of a translation matrix" do
            let transform = translation 5 (-3) 2
                Just inv = inverse transform
                p = mkPoint (-3) 4 5
            Matrix.multT inv p `shouldBe` mkPoint (-8) 7 3

        it "Should not affect vectors" do
            let transform = translation 5 (-3) 2
                v = mkVector (-3) 4 5
            Matrix.multT transform v `shouldBe` v
    describe "Scaling" do
        it "Should apply a scaling matrix to a point" do
            let transform = scaling 2 3 4
                p = mkPoint (-4) 6 8
            Matrix.multT transform p `shouldBe` mkPoint (-8) 18 32
        it "Should apply a scaling matrix to a vector" do
            let transform = scaling 2 3 4
                v = mkVector (-4) 6 8
            Matrix.multT transform v `shouldBe` mkVector (-8) 18 32
        it "Should correctly multiply by the inverse of a scaling matrix" do
            let transform = scaling 2 3 4
                Just inv = inverse transform
                v = mkVector (-4) 6 8
            Matrix.multT inv v `shouldBe` mkVector (-2) 2 2
        it "Should reflect by scaling by negative value" do
            let transform = scaling (-1) 1 1
                p = mkPoint 2 3 4
            Matrix.multT transform p `shouldBe` mkPoint (-2) 3 4
    describe "Rotation" do
        it "Should rotate a point around the x axis" do
            let p = mkPoint 0 1 0
                halfQuarter = rotationX (pi / 4)
                fullQuarter = rotationX (pi / 2)
            Matrix.multT halfQuarter p `shouldBe` mkPoint 0 (sqrt 2 / 2) (sqrt 2 / 2)
            Matrix.multT fullQuarter p `shouldBe` mkPoint 0 0 1
        it "Should rotate in the opposite x axis direction" do
            let p = mkPoint 0 1 0
                halfQuarter = rotationX (pi / 4)
                Just inv = inverse halfQuarter
            Matrix.multT inv p `shouldBe` mkPoint 0 (sqrt 2 / 2) (- sqrt 2 / 2)
        it "Should rotate a point around the y axis" do
            let p = mkPoint 0 0 1
                halfQuarter = rotationY (pi / 4)
                fullQuarter = rotationY (pi / 2)
            Matrix.multT halfQuarter p `shouldBe` mkPoint (sqrt 2 / 2) 0 (sqrt 2 / 2)
            Matrix.multT fullQuarter p `shouldBe` mkPoint 1 0 0
        it "Should rotate a point around the z axis" do
            let p = mkPoint 0 1 0
                halfQuarter = rotationZ (pi / 4)
                fullQuarter = rotationZ (pi / 2)
            Matrix.multT halfQuarter p `shouldBe` mkPoint (- sqrt 2 / 2) (sqrt 2 / 2) 0
            Matrix.multT fullQuarter p `shouldBe` mkPoint (-1) 0 0
