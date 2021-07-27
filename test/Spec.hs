{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Drawing.Canvas (
    Canvas (canvasHeight, canvasWidth),
    mkCanvas,
    pixelAt,
    toList,
    writePixel,
 )
import Drawing.Color (
    Color (colorB, colorG, colorR),
    addC,
    mkColor,
    multC,
    multScalarC,
    subC,
 )
import Drawing.Output (
    FileType (PPMType),
    PPM (PPM),
    PPMHeader (PPMHeader),
    convertToPPM,
    getOutput,
    getRawFile,
 )
import Matrix
import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe)
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

main :: IO ()
main = hspec do
    describe "Tuples, Points, Vectors" do
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
    describe "Color" do
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
    describe "Canvas" do
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
    describe "PPM format" do
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
    describe "Matrices" do
        it "Should construct a 4x4 matrix" do
            let m =
                    mkMatrix
                        4
                        4
                        [ ((0, 0), 1)
                        , ((0, 1), 2)
                        , ((0, 2), 3)
                        , ((0, 3), 4)
                        , ((1, 0), 5.5)
                        , ((1, 1), 6.5)
                        , ((1, 2), 7.5)
                        , ((1, 3), 8.5)
                        , ((2, 0), 9)
                        , ((2, 1), 10)
                        , ((2, 2), 11)
                        , ((2, 3), 12)
                        , ((3, 0), 13.5)
                        , ((3, 1), 14.5)
                        , ((3, 2), 15.5)
                        , ((3, 3), 16.5)
                        ]
            (0, 0) `get` m `shouldBe` Just 1
            (0, 3) `get` m `shouldBe` Just 4
            (1, 0) `get` m `shouldBe` Just 5.5
            (1, 2) `get` m `shouldBe` Just 7.5
            (2, 2) `get` m `shouldBe` Just 11
            (3, 0) `get` m `shouldBe` Just 13.5
            (3, 2) `get` m `shouldBe` Just 15.5
        it "Should construct a 2x2 matrix" do
            let m =
                    mkMatrix
                        2
                        2
                        [ ((0, 0), -3)
                        , ((0, 1), 5)
                        , ((1, 0), 1)
                        , ((1, 1), -2)
                        ]
            (0, 0) `get` m `shouldBe` Just (-3)
            (0, 1) `get` m `shouldBe` Just 5
            (1, 0) `get` m `shouldBe` Just 1
            (1, 1) `get` m `shouldBe` Just (-2)
        it "Should construct a 3x3 matrix" do
            let m =
                    mkMatrix
                        3
                        3
                        [ ((0, 0), -3)
                        , ((0, 1), 5)
                        , ((0, 2), 0)
                        , ((1, 0), 1)
                        , ((1, 1), -2)
                        , ((1, 2), 7)
                        , ((2, 0), 0)
                        , ((2, 1), 1)
                        , ((2, 2), 1)
                        ]
            (0, 0) `get` m `shouldBe` Just (-3)
            (1, 1) `get` m `shouldBe` Just (-2)
            (2, 2) `get` m `shouldBe` Just 1
        it "Should be able to compare identical matrices" do
            let a =
                    mkMatrix
                        4
                        4
                        [ ((0, 0), 1)
                        , ((0, 1), 2)
                        , ((0, 2), 3)
                        , ((0, 3), 4)
                        , ((1, 0), 5)
                        , ((1, 1), 6)
                        , ((1, 2), 7)
                        , ((1, 3), 8)
                        , ((2, 0), 9)
                        , ((2, 1), 8)
                        , ((2, 2), 7)
                        , ((2, 3), 6)
                        , ((3, 0), 5)
                        , ((3, 1), 4)
                        , ((3, 2), 3)
                        , ((3, 3), 2)
                        ]
                b =
                    mkMatrix
                        4
                        4
                        [ ((0, 0), 1)
                        , ((0, 1), 2)
                        , ((0, 2), 3)
                        , ((0, 3), 4)
                        , ((1, 0), 5)
                        , ((1, 1), 6)
                        , ((1, 2), 7)
                        , ((1, 3), 8)
                        , ((2, 0), 9)
                        , ((2, 1), 8)
                        , ((2, 2), 7)
                        , ((2, 3), 6)
                        , ((3, 0), 5)
                        , ((3, 1), 4)
                        , ((3, 2), 3)
                        , ((3, 3), 2)
                        ]
            a `shouldBe` b

        it "Should be able to compare non-identical matrices" do
            let a =
                    mkMatrix
                        4
                        4
                        [ ((0, 0), 1)
                        , ((0, 1), 2)
                        , ((0, 2), 3)
                        , ((0, 3), 4)
                        , ((1, 0), 5)
                        , ((1, 1), 6)
                        , ((1, 2), 7)
                        , ((1, 3), 8)
                        , ((2, 0), 9)
                        , ((2, 1), 8)
                        , ((2, 2), 7)
                        , ((2, 3), 6)
                        , ((3, 0), 5)
                        , ((3, 1), 4)
                        , ((3, 2), 3)
                        , ((3, 3), 2)
                        ]
                b =
                    mkMatrix
                        4
                        4
                        [ ((0, 0), 2)
                        , ((0, 1), 3)
                        , ((0, 2), 4)
                        , ((0, 3), 5)
                        , ((1, 0), 6)
                        , ((1, 1), 7)
                        , ((1, 2), 8)
                        , ((1, 3), 9)
                        , ((2, 0), 8)
                        , ((2, 1), 7)
                        , ((2, 2), 6)
                        , ((2, 3), 5)
                        , ((3, 0), 4)
                        , ((3, 1), 3)
                        , ((3, 2), 2)
                        , ((3, 3), 1)
                        ]
            a `shouldNotBe` b
        describe "Multiplication" do
            it "Should multiply two 4x4 matrices" do
                let m1 =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), 1)
                            , ((0, 1), 2)
                            , ((0, 2), 3)
                            , ((0, 3), 4)
                            , ((1, 0), 5)
                            , ((1, 1), 6)
                            , ((1, 2), 7)
                            , ((1, 3), 8)
                            , ((2, 0), 9)
                            , ((2, 1), 8)
                            , ((2, 2), 7)
                            , ((2, 3), 6)
                            , ((3, 0), 5)
                            , ((3, 1), 4)
                            , ((3, 2), 3)
                            , ((3, 3), 2)
                            ]
                    m2 =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), -2)
                            , ((0, 1), 1)
                            , ((0, 2), 2)
                            , ((0, 3), 3)
                            , ((1, 0), 3)
                            , ((1, 1), 2)
                            , ((1, 2), 1)
                            , ((1, 3), -1)
                            , ((2, 0), 4)
                            , ((2, 1), 3)
                            , ((2, 2), 6)
                            , ((2, 3), 5)
                            , ((3, 0), 1)
                            , ((3, 1), 2)
                            , ((3, 2), 7)
                            , ((3, 3), 8)
                            ]
                    expected =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), 20)
                            , ((0, 1), 22)
                            , ((0, 2), 50)
                            , ((0, 3), 48)
                            , ((1, 0), 44)
                            , ((1, 1), 54)
                            , ((1, 2), 114)
                            , ((1, 3), 108)
                            , ((2, 0), 40)
                            , ((2, 1), 58)
                            , ((2, 2), 110)
                            , ((2, 3), 102)
                            , ((3, 0), 16)
                            , ((3, 1), 26)
                            , ((3, 2), 46)
                            , ((3, 3), 42)
                            ]
                multM m1 m2 `shouldBe` expected
            it "Should multiply by a tuple" do
                let m1 =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), 1)
                            , ((0, 1), 2)
                            , ((0, 2), 3)
                            , ((0, 3), 4)
                            , ((1, 0), 2)
                            , ((1, 1), 4)
                            , ((1, 2), 4)
                            , ((1, 3), 2)
                            , ((2, 0), 8)
                            , ((2, 1), 6)
                            , ((2, 2), 4)
                            , ((2, 3), 1)
                            , ((3, 0), 0)
                            , ((3, 1), 0)
                            , ((3, 2), 0)
                            , ((3, 3), 1)
                            ]
                    t = T (1, 2, 3, 1)
                Matrix.multT m1 t `shouldBe` T (18, 24, 33, 1)
            it "Should multiply by identity matrix" do
                let m =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), 0)
                            , ((0, 1), 1)
                            , ((0, 2), 2)
                            , ((0, 3), 4)
                            , ((1, 0), 1)
                            , ((1, 1), 2)
                            , ((1, 2), 4)
                            , ((1, 3), 8)
                            , ((2, 0), 2)
                            , ((2, 1), 4)
                            , ((2, 2), 8)
                            , ((2, 3), 16)
                            , ((3, 0), 4)
                            , ((3, 1), 8)
                            , ((3, 2), 16)
                            , ((3, 3), 32)
                            ]
                multM m (identityMatrix 4 4) `shouldBe` m
        describe "Transposition" do
            it "Should transpose a matrix" do
                let m =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), 0)
                            , ((0, 1), 9)
                            , ((0, 2), 3)
                            , ((0, 3), 0)
                            , ((1, 0), 9)
                            , ((1, 1), 8)
                            , ((1, 2), 0)
                            , ((1, 3), 8)
                            , ((2, 0), 1)
                            , ((2, 1), 8)
                            , ((2, 2), 5)
                            , ((2, 3), 3)
                            , ((3, 0), 0)
                            , ((3, 1), 0)
                            , ((3, 2), 5)
                            , ((3, 3), 8)
                            ]
                    expected =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), 0)
                            , ((0, 1), 9)
                            , ((0, 2), 1)
                            , ((0, 3), 0)
                            , ((1, 0), 9)
                            , ((1, 1), 8)
                            , ((1, 2), 8)
                            , ((1, 3), 0)
                            , ((2, 0), 3)
                            , ((2, 1), 0)
                            , ((2, 2), 5)
                            , ((2, 3), 5)
                            , ((3, 0), 0)
                            , ((3, 1), 8)
                            , ((3, 2), 3)
                            , ((3, 3), 8)
                            ]
                transposeM m `shouldBe` expected
            it "Should transpose the identity matrix into itself" do
                transposeM (identityMatrix 4 4) `shouldBe` identityMatrix 4 4
        describe "Inverting Matrices" do
            it "Should calculate the determinant of a 2x2 matrix" do
                let m = mkMatrix 2 2 [((0, 0), 1), ((0, 1), 5), ((1, 0), -3), ((1, 1), 2)]

                determinant m `shouldBe` 17
            it "Should calculate proper submatrix of a 3x3 matrix" do
                let m =
                        mkMatrix
                            3
                            3
                            [ ((0, 0), 1)
                            , ((0, 1), 5)
                            , ((0, 2), 0)
                            , ((1, 0), -3)
                            , ((1, 1), 2)
                            , ((1, 2), 7)
                            , ((2, 0), 0)
                            , ((2, 1), 6)
                            , ((2, 2), -3)
                            ]
                    expected = mkMatrix 2 2 [((0, 0), -3), ((0, 1), 2), ((1, 0), 0), ((1, 1), 6)]
                submatrix 0 2 m `shouldBe` expected
            it "Should calculate proper submatrix of a 4x4 matrix" do
                let m =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), -6)
                            , ((0, 1), 1)
                            , ((0, 2), 1)
                            , ((0, 3), 6)
                            , ((1, 0), -8)
                            , ((1, 1), 5)
                            , ((1, 2), 8)
                            , ((1, 3), 6)
                            , ((2, 0), -1)
                            , ((2, 1), 0)
                            , ((2, 2), 8)
                            , ((2, 3), 2)
                            , ((3, 0), -7)
                            , ((3, 1), 1)
                            , ((3, 2), -1)
                            , ((3, 3), 1)
                            ]
                    expected =
                        mkMatrix
                            3
                            3
                            [ ((0, 0), -6)
                            , ((0, 1), 1)
                            , ((0, 2), 6)
                            , ((1, 0), -8)
                            , ((1, 1), 8)
                            , ((1, 2), 6)
                            , ((2, 0), -7)
                            , ((2, 1), -1)
                            , ((2, 2), 1)
                            ]

                submatrix 2 1 m `shouldBe` expected
            it "Should calculate the minor of a 3x3 matrix" do
                let a =
                        mkMatrix
                            3
                            3
                            [ ((0, 0), 3)
                            , ((0, 1), 5)
                            , ((0, 2), 0)
                            , ((1, 0), 2)
                            , ((1, 1), -1)
                            , ((1, 2), -7)
                            , ((2, 0), 6)
                            , ((2, 1), -1)
                            , ((2, 2), 5)
                            ]
                    b = submatrix 1 0 a

                determinant b `shouldBe` 25
                minor 1 0 a `shouldBe` 25

            it "Should calculate the cofactor of a 3x3 matrix" do
                let a =
                        mkMatrix
                            3
                            3
                            [ ((0, 0), 3)
                            , ((0, 1), 5)
                            , ((0, 2), 0)
                            , ((1, 0), 2)
                            , ((1, 1), -1)
                            , ((1, 2), -7)
                            , ((2, 0), 6)
                            , ((2, 1), -1)
                            , ((2, 2), 5)
                            ]

                minor 0 0 a `shouldBe` -12
                cofactor 0 0 a `shouldBe` -12

                minor 1 0 a `shouldBe` 25
                cofactor 1 0 a `shouldBe` -25
            it "Should calculate the determinant of a 3x3 matrix" do
                let a =
                        mkMatrix
                            3
                            3
                            [ ((0, 0), 1)
                            , ((0, 1), 2)
                            , ((0, 2), 6)
                            , ((1, 0), -5)
                            , ((1, 1), 8)
                            , ((1, 2), -4)
                            , ((2, 0), 2)
                            , ((2, 1), 6)
                            , ((2, 2), 4)
                            ]
                cofactor 0 0 a `shouldBe` 56
                cofactor 0 1 a `shouldBe` 12
                cofactor 0 2 a `shouldBe` -46
                determinant a `shouldBe` -196

            it "Should calculate the determinant of a 4x4 matrix" do
                let a =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), -2)
                            , ((0, 1), -8)
                            , ((0, 2), 3)
                            , ((0, 3), 5)
                            , ((1, 0), -3)
                            , ((1, 1), 1)
                            , ((1, 2), 7)
                            , ((1, 3), 3)
                            , ((2, 0), 1)
                            , ((2, 1), 2)
                            , ((2, 2), -9)
                            , ((2, 3), 6)
                            , ((3, 0), -6)
                            , ((3, 1), 7)
                            , ((3, 2), 7)
                            , ((3, 3), -9)
                            ]
                cofactor 0 0 a `shouldBe` 690
                cofactor 0 1 a `shouldBe` 447
                cofactor 0 2 a `shouldBe` 210
                cofactor 0 3 a `shouldBe` 51
                determinant a `shouldBe` -4071

            it "Should determine an invertible matrix" do
                let a =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), 6)
                            , ((0, 1), 4)
                            , ((0, 2), 4)
                            , ((0, 3), 4)
                            , ((1, 0), 5)
                            , ((1, 1), 5)
                            , ((1, 2), 7)
                            , ((1, 3), 6)
                            , ((2, 0), 4)
                            , ((2, 1), -9)
                            , ((2, 2), 3)
                            , ((2, 3), -7)
                            , ((3, 0), 9)
                            , ((3, 1), 1)
                            , ((3, 2), 7)
                            , ((3, 3), -6)
                            ]
                determinant a `shouldBe` -2120
                invertible a `shouldBe` Invertible

            it "Should determine an uninvertible matrix" do
                let a =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), -4)
                            , ((0, 1), 2)
                            , ((0, 2), -2)
                            , ((0, 3), -3)
                            , ((1, 0), 9)
                            , ((1, 1), 6)
                            , ((1, 2), 2)
                            , ((1, 3), 6)
                            , ((2, 0), 0)
                            , ((2, 1), -5)
                            , ((2, 2), 1)
                            , ((2, 3), -5)
                            , ((3, 0), 0)
                            , ((3, 1), 0)
                            , ((3, 2), 0)
                            , ((3, 3), 0)
                            ]
                determinant a `shouldBe` 0
                invertible a `shouldBe` NotInvertible

            it "Should calculate the inverse of a matrix" do
                let a =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), -5)
                            , ((0, 1), 2)
                            , ((0, 2), 6)
                            , ((0, 3), -8)
                            , ((1, 0), 1)
                            , ((1, 1), -5)
                            , ((1, 2), 1)
                            , ((1, 3), 8)
                            , ((2, 0), 7)
                            , ((2, 1), 7)
                            , ((2, 2), -6)
                            , ((2, 3), -7)
                            , ((3, 0), 1)
                            , ((3, 1), -3)
                            , ((3, 2), 7)
                            , ((3, 3), 4)
                            ]
                    (Just b) = inverse a
                    expected =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), 0.21805)
                            , ((0, 1), 0.45113)
                            , ((0, 2), 0.24060)
                            , ((0, 3), -0.04511)
                            , ((1, 0), -0.80827)
                            , ((1, 1), -1.45677)
                            , ((1, 2), -0.44361)
                            , ((1, 3), 0.52068)
                            , ((2, 0), -0.07895)
                            , ((2, 1), -0.22368)
                            , ((2, 2), -0.05263)
                            , ((2, 3), 0.19737)
                            , ((3, 0), -0.52256)
                            , ((3, 1), -0.81391)
                            , ((3, 2), -0.30075)
                            , ((3, 3), 0.30639)
                            ]
                determinant a `shouldBe` 532
                cofactor 2 3 a `shouldBe` -160
                (3, 2) `get` b `shouldBe` (Just $ -160 / 532)
                cofactor 3 2 a `shouldBe` 105
                (2, 3) `get` b `shouldBe` (Just $ 105 / 532)
                b `shouldBe` expected

            it "Should calculate the inverse of another matrix" do
                let a =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), 8)
                            , ((0, 1), -5)
                            , ((0, 2), 9)
                            , ((0, 3), 2)
                            , ((1, 0), 7)
                            , ((1, 1), 5)
                            , ((1, 2), 6)
                            , ((1, 3), 1)
                            , ((2, 0), -6)
                            , ((2, 1), 0)
                            , ((2, 2), 9)
                            , ((2, 3), 6)
                            , ((3, 0), -3)
                            , ((3, 1), 0)
                            , ((3, 2), -9)
                            , ((3, 3), -4)
                            ]
                    (Just b) = inverse a
                    expected =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), -0.15385)
                            , ((0, 1), -0.15385)
                            , ((0, 2), -0.28205)
                            , ((0, 3), -0.53846)
                            , ((1, 0), -0.07692)
                            , ((1, 1), 0.12308)
                            , ((1, 2), 0.02564)
                            , ((1, 3), 0.03077)
                            , ((2, 0), 0.35897)
                            , ((2, 1), 0.35897)
                            , ((2, 2), 0.43590)
                            , ((2, 3), 0.92308)
                            , ((3, 0), -0.69231)
                            , ((3, 1), -0.69231)
                            , ((3, 2), -0.76923)
                            , ((3, 3), -1.92308)
                            ]
                b `shouldBe` expected

            it "Should calculate the inverse of a third matrix" do
                let a =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), 9)
                            , ((0, 1), 3)
                            , ((0, 2), 0)
                            , ((0, 3), 9)
                            , ((1, 0), -5)
                            , ((1, 1), -2)
                            , ((1, 2), -6)
                            , ((1, 3), -3)
                            , ((2, 0), -4)
                            , ((2, 1), 9)
                            , ((2, 2), 6)
                            , ((2, 3), 4)
                            , ((3, 0), -7)
                            , ((3, 1), 6)
                            , ((3, 2), 6)
                            , ((3, 3), 2)
                            ]
                    (Just b) = inverse a
                    expected =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), -0.04074)
                            , ((0, 1), -0.07778)
                            , ((0, 2), 0.14444)
                            , ((0, 3), -0.22222)
                            , ((1, 0), -0.07778)
                            , ((1, 1), 0.03333)
                            , ((1, 2), 0.36667)
                            , ((1, 3), -0.33333)
                            , ((2, 0), -0.02901)
                            , ((2, 1), -0.14630)
                            , ((2, 2), -0.10926)
                            , ((2, 3), 0.12963)
                            , ((3, 0), 0.17778)
                            , ((3, 1), 0.06667)
                            , ((3, 2), -0.26667)
                            , ((3, 3), 0.33333)
                            ]
                b `shouldBe` expected

            it "Should correctly multiply by a product of inverse" do
                let a =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), 3)
                            , ((0, 1), -9)
                            , ((0, 2), 7)
                            , ((0, 3), 3)
                            , ((1, 0), 3)
                            , ((1, 1), -8)
                            , ((1, 2), 2)
                            , ((1, 3), -9)
                            , ((2, 0), -4)
                            , ((2, 1), 4)
                            , ((2, 2), 4)
                            , ((2, 3), 1)
                            , ((3, 0), -6)
                            , ((3, 1), 5)
                            , ((3, 2), -1)
                            , ((3, 3), 1)
                            ]
                    b =
                        mkMatrix
                            4
                            4
                            [ ((0, 0), 8)
                            , ((0, 1), 2)
                            , ((0, 2), 2)
                            , ((0, 3), 2)
                            , ((1, 0), 3)
                            , ((1, 1), -1)
                            , ((1, 2), 7)
                            , ((1, 3), 0)
                            , ((2, 0), 7)
                            , ((2, 1), 0)
                            , ((2, 2), 5)
                            , ((2, 3), 4)
                            , ((3, 0), 6)
                            , ((3, 1), -2)
                            , ((3, 2), 0)
                            , ((3, 3), 5)
                            ]
                    c = multM a b
                    Just b' = inverse b
                multM c b' `shouldBe` a
