{-# LANGUAGE BlockArguments #-}

module ObjectSpec where

import Data.Function
import Matrix
import Object
import Test.Hspec
import Transformation
import Tuple

spec :: Spec
spec = describe "Sphere" do
    it "Should have the correct default transformation" do
        let s = mkSphere
        sphereTransformation s `shouldBe` identityMatrix 4 4

    it "Should set the transformation" do
        let t = translation 2 3 4
            s = mkSphere & setTransform t

        sphereTransformation s `shouldBe` t

    describe "Normals" do
        it "Should compute the normal on a sphere at a point on the x axis" do
            let s = mkSphere
                n = normalAt (mkPoint 1 0 0) s

            n `shouldBe` mkVector 1 0 0
        it "Should compute the normal on a sphere at a point on the y axis" do
            let s = mkSphere
                n = normalAt (mkPoint 0 1 0) s

            n `shouldBe` mkVector 0 1 0
        it "Should compute the normal on a sphere at a point on the z axis" do
            let s = mkSphere
                n = normalAt (mkPoint 0 0 1) s

            n `shouldBe` mkVector 0 0 1
        it "Should compute the normal on a sphere at a non-axial point" do
            let s = mkSphere
                n = normalAt (mkPoint (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)) s

            n `shouldBe` mkVector (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)
        it "Should compute normals as a normalized vector" do
            let s = mkSphere
                n = normalAt (mkPoint (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)) s

            n `shouldBe` normalizeT n
        it "Should compute the normal on a translated sphere" do
            let s = mkSphere & setTransform (translation 0 1 0)
                n = normalAt (mkPoint 0 1.70711 (-0.70711)) s

            n `shouldBe` mkVector 0 0.70711 (-0.70711)
        it "Should compute the normal on a transformed sphere" do
            let s = mkSphere & setTransform (scaling 1 0.5 1 `multM` rotationZ (pi / 5))
                n = normalAt (mkPoint 0 (sqrt 2 / 2) (- sqrt 2 / 2)) s

            n `shouldBe` mkVector 0 0.97014 (-0.24254)