{-# LANGUAGE BlockArguments #-}

module MatrixSpec where

import Matrix (
    Invertibility (Invertible, NotInvertible),
    cofactor,
    determinant,
    get,
    identityMatrix,
    inverse,
    invertible,
    minor,
    mkMatrix,
    multM,
    multT,
    submatrix,
    transposeM,
 )
import qualified Matrix
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Tuple (T (T))

spec :: Spec
spec = describe "Matrices" do
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