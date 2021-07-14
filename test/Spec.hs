{-# LANGUAGE BlockArguments #-}
import           Lib
import           Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec do
    describe "Tuples, Points, Vectors" do
        describe "Conversion" do
            it "Converting a point to tuple has 1 in the final spot" do
                toTuple (mkPoint 4.3 (-4.2) 3.1) `shouldBe` (MyNum 4.3, MyNum (-4.2), MyNum 3.1, MyNum 1)
            it "Converting a vector to tuple has 0 in the final spot" do
                toTuple (mkVector 4.3 (-4.2) 3.1) `shouldBe` (MyNum 4.3, MyNum (-4.2), MyNum 3.1, MyNum 0)
        describe "Operations" do
            describe "Addition" do
                it "Should add two tuples" do
                    let a1 = mkPoint 3 (-2) 5
                        a2 = mkVector (-2) 3 1
                        expected = Just $ mkPoint 1 1 6
                        actual = a1 `addT` a2
                    actual `shouldBe` expected
                it "Should not add two points" do
                    let a1 = mkPoint 3 (-2) 5
                        a2 = mkPoint (-2) 3 1
                        expected = Nothing
                        actual = a1 `addT` a2
                    actual `shouldBe` expected

            describe "Subtraction" do
                it "Should subtract two points" do
                    let p1 = mkPoint 3 2 1
                        p2 = mkPoint 5 6 7
                        expected = Just $ mkVector (-2) (-4) (-6)
                        actual = p1 `subtractT` p2
                    actual `shouldBe` expected
                it "Should subtract a vector from a point" do
                    let p = mkPoint 3 2 1
                        v = mkVector 5 6 7
                        expected = Just $ mkPoint (-2) (-4) (-6)
                        actual = p `subtractT` v
                    actual `shouldBe` expected
                it "Should subtract two vectors" do
                    let v1 = mkVector 3 2 1
                        v2 = mkVector 5 6 7
                        expected = Just $ mkVector (-2) (-4) (-6)
                        actual = v1 `subtractT` v2
                    actual `shouldBe` expected
                it "Should not be able to subtract point from vector" do
                    let p = mkPoint 3 2 1
                        v = mkVector 5 6 7
                        expected = Nothing
                        actual = v `subtractT` p
                    actual `shouldBe` expected
                it "Should subtract a vector from the zero vector" do
                    let zero = mkVector 0 0 0
                        v = mkVector 1 (-2) 3
                        expected = Just $ mkVector (-1) 2 (-3)
                        actual = zero `subtractT` v
                    actual `shouldBe` expected

            describe "Negation" do
                it "Should negate a vector" do
                    let v = mkVector 1 (-2) 3
                        expected = mkVector (-1) 2 (-3)
                        actual = negateT v
                    actual `shouldBe` expected
                it "Should negate a point" do
                    let v = mkPoint 1 (-2) 3
                        expected = mkPoint (-1) 2 (-3)
                        actual = negateT v
                    actual `shouldBe` expected

            describe "Multiplication" do
                it "Should multiply a point" do
                    let p = mkPoint 1 (-2) 3
                        expected = mkPoint 3.5 (-7) 10.5
                        actual = p `multiplyT` 3.5
                    actual `shouldBe` expected
                it "Should multiply a vectors" do
                    let v = mkVector 1 (-2) 3
                        expected = mkVector 3.5 (-7) 10.5
                        actual = v `multiplyT` 3.5
                    actual `shouldBe` expected
                it "Should multiply a point by a fraction" do
                    let p = mkPoint 1 (-2) 3
                        expected = mkPoint 0.5 (-1) 1.5
                        actual = p `multiplyT` 0.5
                    actual `shouldBe` expected

            describe "Division" do
                it "Should divide a point" do
                    let p = mkPoint 1 (-2) 3
                        expected = Just $ mkPoint 0.5 (-1) 1.5
                        actual = p `divideT` 2
                    actual `shouldBe` expected
                it "Should divide a vectors" do
                    let v = mkVector 1 (-2) 3
                        expected = Just $ mkVector 0.5 (-1) 1.5
                        actual = v `divideT` 2
                    actual `shouldBe` expected
                it "Should not divide by zero" do
                    let v = mkVector 1 (-2) 3
                        expected = Nothing
                        actual = v `divideT` 0
                    actual `shouldBe` expected

        describe "Magnitude" do
            it "Should compute the magnitude of vector (1,0,0)" do
                let v = mkVector 1 0 0
                    expected = Just $ MyNum 1
                    actual = magnitudeT v
                actual `shouldBe` expected
            it "Should compute the magnitude of vector (0,1,0)" do
                let v = mkVector 0 1 0
                    expected = Just $ MyNum 1
                    actual = magnitudeT v
                actual `shouldBe` expected
            it "Should compute the magnitude of vector (0,0,1)" do
                let v = mkVector 0 0 1
                    expected = Just $ MyNum 1
                    actual = magnitudeT v
                actual `shouldBe` expected
            it "Should compute the magnitude of vector (1,2,3)" do
                let v = mkVector 1 2 3
                    expected = Just $ MyNum (sqrt 14)
                    actual = magnitudeT v
                actual `shouldBe` expected
            it "Should compute the magnitude of vector (-1,-2,-3)" do
                let v = mkVector (-1) (-2) (-3)
                    expected = Just $ MyNum (sqrt 14)
                    actual = magnitudeT v
                actual `shouldBe` expected

            describe "Normalization" do
                pure ()

