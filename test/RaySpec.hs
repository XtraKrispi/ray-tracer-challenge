{-# LANGUAGE BlockArguments #-}

module RaySpec where

import Data.Function
import Intersections
import Object
import Ray
import Test.Hspec
import Transformation
import Tuple

spec :: Spec
spec = describe "Rays" do
    it "Should create and query a ray properly" do
        let o = mkPoint 1 2 3
            d = mkVector 4 5 6
            r = mkRay o d

        origin r `shouldBe` o
        direction r `shouldBe` d
    it "Should compute a point from a distance" do
        let r = mkRay (mkPoint 2 3 4) (mkVector 1 0 0)

        position 0 r `shouldBe` mkPoint 2 3 4
        position 1 r `shouldBe` mkPoint 3 3 4
        position (-1) r `shouldBe` mkPoint 1 3 4
        position 2.5 r `shouldBe` mkPoint 4.5 3 4
    describe "Sphere intersection" do
        it "Should intersect a sphere at two points" do
            let r = mkRay (mkPoint 0 0 (-5)) (mkVector 0 0 1)
                s = mkSphere
                xs = r `intersect` s

            intersectionTValue <$> xs `shouldBe` [4, 6]

        it "Should intersect with a sphere at a tangent" do
            let r = mkRay (mkPoint 0 1 (-5)) (mkVector 0 0 1)
                s = mkSphere
                xs = r `intersect` s

            intersectionTValue <$> xs `shouldBe` [5, 5]

        it "Should miss a sphere" do
            let r = mkRay (mkPoint 0 2 (-5)) (mkVector 0 0 1)
                s = mkSphere
                xs = r `intersect` s

            length xs `shouldBe` 0

        it "Should originate within a sphere" do
            let r = mkRay (mkPoint 0 0 0) (mkVector 0 0 1)
                s = mkSphere
                xs = r `intersect` s

            intersectionTValue <$> xs `shouldBe` [-1, 1]

        it "Should work with a sphere behind the ray" do
            let r = mkRay (mkPoint 0 0 5) (mkVector 0 0 1)
                s = mkSphere
                xs = r `intersect` s

            intersectionTValue <$> xs `shouldBe` [-6, -4]
        it "Should set the object on the intersection" do
            let r = mkRay (mkPoint 0 0 (-5)) (mkVector 0 0 1)
                s = mkSphere
                xs = r `intersect` s

            length xs `shouldBe` 2
            intersectionObject (head xs) `shouldBe` s
            intersectionObject (xs !! 1) `shouldBe` s

        it "Should intersect a scaled sphere with a ray" do
            let r = mkRay (mkPoint 0 0 (-5)) (mkVector 0 0 1)
                s = mkSphere & setTransform (scaling 2 2 2)
                xs = r `intersect` s

            intersectionTValue <$> xs `shouldBe` [3, 7]

        it "Should intersect a translated sphere with a ray" do
            let r = mkRay (mkPoint 0 0 (-5)) (mkVector 0 0 1)
                s = mkSphere & setTransform (translation 5 0 0)
                xs = r `intersect` s

            length xs `shouldBe` 0

    describe "Ray transformations" do
        it "Should translate a ray" do
            let r = mkRay (mkPoint 1 2 3) (mkVector 0 1 0)
                m = translation 3 4 5
                r2 = transform m r

            origin r2 `shouldBe` mkPoint 4 6 8
            direction r2 `shouldBe` mkVector 0 1 0

        it "Should scale a ray" do
            let r = mkRay (mkPoint 1 2 3) (mkVector 0 1 0)
                m = scaling 2 3 4
                r2 = transform m r

            origin r2 `shouldBe` mkPoint 2 6 12
            direction r2 `shouldBe` mkVector 0 3 0