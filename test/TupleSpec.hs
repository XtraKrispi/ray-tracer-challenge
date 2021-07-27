{-# LANGUAGE BlockArguments #-}

module TupleSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Tuple (
    T (T),
    addT,
    crossProduct,
    divideScalar,
    dotProduct,
    magnitudeT,
    mkPoint,
    mkVector,
    multiplyScalar,
    negateT,
    normalizeT,
    subtractT,
    toTuple,
 )

spec :: Spec
spec = describe "Tuples, Points, Vectors" do
    describe "Conversion" do
        it "Converting a point to tuple has 1 in the final spot" do
            toTuple (mkPoint 4.3 (-4.2) 3.1) `shouldBe` (4.3, -4.2, 3.1, 1)
        it "Converting a vector to tuple has 0 in the final spot" do
            toTuple (mkVector 4.3 (-4.2) 3.1) `shouldBe` (4.3, -4.2, 3.1, 0)
    describe "Operations" do
        describe "Addition" do
            it "Should add two tuples" do
                let a1 = mkPoint 3 (-2) 5
                    a2 = mkVector (-2) 3 1
                    expected = mkPoint 1 1 6
                    actual = a1 `addT` a2
                actual `shouldBe` expected

        describe "Subtraction" do
            it "Should subtract two points" do
                let p1 = mkPoint 3 2 1
                    p2 = mkPoint 5 6 7
                    expected = mkVector (-2) (-4) (-6)
                    actual = p1 `subtractT` p2
                actual `shouldBe` expected
            it "Should subtract a vector from a point" do
                let p = mkPoint 3 2 1
                    v = mkVector 5 6 7
                    expected = mkPoint (-2) (-4) (-6)
                    actual = p `subtractT` v
                actual `shouldBe` expected
            it "Should subtract two vectors" do
                let v1 = mkVector 3 2 1
                    v2 = mkVector 5 6 7
                    expected = mkVector (-2) (-4) (-6)
                    actual = v1 `subtractT` v2
                actual `shouldBe` expected
            it "Should subtract a vector from the zero vector" do
                let zero = mkVector 0 0 0
                    v = mkVector 1 (-2) 3
                    expected = mkVector (-1) 2 (-3)
                    actual = zero `subtractT` v
                actual `shouldBe` expected

        describe "Negation" do
            it "Should negate a point" do
                let v = mkPoint 1 (-2) 3
                    expected = (-1, 2, -3, -1)
                    actual = toTuple $ negateT v
                actual `shouldBe` expected

        describe "Multiplication" do
            it "Should multiply a tuple by a scalar" do
                let p = T (1, -2, 3, -4)
                    expected = T (3.5, -7, 10.5, -14)
                    actual = p `multiplyScalar` 3.5
                actual `shouldBe` expected
            it "Should multiply a tuple by a fraction" do
                let p = T (1, -2, 3, -4)
                    expected = T (0.5, -1, 1.5, -2)
                    actual = p `multiplyScalar` 0.5
                actual `shouldBe` expected

        describe "Division" do
            it "Should divide a tuple by a scalar" do
                let p = T (1, -2, 3, -4)
                    expected = Just $ T (0.5, -1, 1.5, -2)
                    actual = p `divideScalar` 2
                actual `shouldBe` expected

    describe "Magnitude" do
        it "Should compute the magnitude of vector (1,0,0)" do
            let v = mkVector 1 0 0
                expected = 1
                actual = magnitudeT v
            actual `shouldBe` expected
        it "Should compute the magnitude of vector (0,1,0)" do
            let v = mkVector 0 1 0
                expected = 1
                actual = magnitudeT v
            actual `shouldBe` expected
        it "Should compute the magnitude of vector (0,0,1)" do
            let v = mkVector 0 0 1
                expected = 1
                actual = magnitudeT v
            actual `shouldBe` expected
        it "Should compute the magnitude of vector (1,2,3)" do
            let v = mkVector 1 2 3
                expected = sqrt 14
                actual = magnitudeT v
            actual `shouldBe` expected
        it "Should compute the magnitude of vector (-1,-2,-3)" do
            let v = mkVector (-1) (-2) (-3)
                expected = sqrt 14
                actual = magnitudeT v
            actual `shouldBe` expected

    describe "Normalization" do
        it "Should normalize vector (4,0,0)" do
            let v = mkVector 4 0 0
                expected = mkVector 1 0 0
                actual = normalizeT v
            actual `shouldBe` expected
        it "Should normalize vector (1,2,3)" do
            let v = mkVector 1 2 3
                expected = mkVector 0.26726 0.53452 0.80178
                actual = normalizeT v
            actual `shouldBe` expected
        it "Should have a magnitude of 1" do
            let v = mkVector 1 2 3
                expected = 1
                actual = magnitudeT $ normalizeT v
            actual `shouldBe` expected

    describe "Dot Product" do
        it "Should produce a correct dot product" do
            let v1 = mkVector 1 2 3
                v2 = mkVector 2 3 4
                expected = 20
                actual = dotProduct v1 v2
            actual `shouldBe` expected

    describe "Cross Product" do
        it "Should product a correct cross product" do
            let v1 = mkVector 1 2 3
                v2 = mkVector 2 3 4
                expected1 = mkVector (-1) 2 (-1)
                expected2 = mkVector 1 (-2) 1
                actual1 = crossProduct v1 v2
                actual2 = crossProduct v2 v1
            actual1 `shouldBe` expected1
            actual2 `shouldBe` expected2