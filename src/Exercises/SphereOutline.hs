module Exercises.SphereOutline where

import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Function ((&))
import Drawing.Canvas (Canvas, mkCanvasWithData)
import Drawing.Color (mkColor)
import Intersections (hit)
import Matrix (multM)
import Object (mkSphere, setTransform)
import Ray (intersect, mkRay)
import Transformation (scaling, shearing)
import Tuple (mkPoint, normalizeT, subtractT)

draw :: Canvas
draw =
    let wallSize :: Double
        wallSize = 7
        wallZ = 10
        rayOrigin = mkPoint 0 0 (-5)
        canvasPixels = 100
        black = mkColor 0 0 0
        color = mkColor 1 0 0
        shape = mkSphere & setTransform (shearing 1 0 0 0 0 0 `multM` scaling 0.5 1 1)
        pixelSize = wallSize / fromIntegral canvasPixels
        half = wallSize / 2
        allCoords = [(x, y) | x <- [0 .. canvasPixels - 1], y <- [0 .. canvasPixels - 1]]
        op (x, y) =
            let worldY = half - pixelSize * fromIntegral y
                worldX = - half + pixelSize * fromIntegral x
                position = mkPoint worldX worldY wallZ
                r = mkRay rayOrigin $ normalizeT $ subtractT position rayOrigin
                xs = intersect r shape
             in case hit xs of
                    Just _ -> ((x, y), color)
                    Nothing -> ((x, y), black)
     in mkCanvasWithData canvasPixels canvasPixels ((op <$> allCoords) `using` parList rdeepseq)