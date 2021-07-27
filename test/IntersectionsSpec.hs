{-# LANGUAGE BlockArguments #-}

module IntersectionsSpec where

import Intersections
import Object
import Ray
import Test.Hspec

spec :: Spec
spec = describe "Intersections" do
    it "Should encapsulate a t and object" do
        let s = mkSphere
            i = intersection 3.5 s

        intersectionTValue i `shouldBe` 3.5
        intersectionObject i `shouldBe` s
    describe "The hit" do
        it "Should correctly calculate the hit with positive ts" do
            let s = mkSphere
                i1 = intersection 1 s
                i2 = intersection 2 s
                i = hit [i2, i1]
            i `shouldBe` Just i1

        it "Should calculate the hit corerctly when some intersections have negative t" do
            let s = mkSphere
                i1 = intersection (-1) s
                i2 = intersection 1 s
                i = hit [i2, i1]
            i `shouldBe` Just i2

        it "Should calculate the hit correctly when all intersections have negative t" do
            let s = mkSphere
                i1 = intersection (-2) s
                i2 = intersection (-1) s
                i = hit [i2, i1]
            i `shouldBe` Nothing

        it "Should ensure that the hit is always the lowest non-negative intersection" do
            let s = mkSphere
                i1 = intersection 5 s
                i2 = intersection 7 s
                i3 = intersection (-3) s
                i4 = intersection 2 s
                i = hit [i1, i2, i3, i4]

            i `shouldBe` Just i4