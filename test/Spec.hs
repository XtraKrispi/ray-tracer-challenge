{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified CanvasSpec
import qualified ColorSpec
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
import qualified IntersectionsSpec
import Matrix
import qualified MatrixSpec
import qualified ObjectSpec
import qualified PPMSpec
import qualified RaySpec
import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe)
import qualified TransformationSpec
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
import qualified TupleSpec

main :: IO ()
main = hspec do
    TupleSpec.spec
    ColorSpec.spec
    CanvasSpec.spec
    PPMSpec.spec
    MatrixSpec.spec
    TransformationSpec.spec
    RaySpec.spec
    IntersectionsSpec.spec
    ObjectSpec.spec