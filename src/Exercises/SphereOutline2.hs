module Exercises.SphereOutline2 where

import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Function ((&))
import Drawing.Canvas (Canvas, mkCanvasWithData)
import Drawing.Color (mkColor)
import Intersections (Intersection (Intersection), hit)
import Light
import Material
import Matrix (multM)
import Object (Object (sphereMaterial), mkSphere, normalAt, setMaterial, setTransform)
import Ray (Ray (direction), intersect, mkRay, position)
import Transformation (scaling, shearing)
import Tuple (mkPoint, negateT, normalizeT, subtractT)

draw :: Canvas
draw =
    let wallSize :: Double
        wallSize = 7
        wallZ = 10
        rayOrigin = mkPoint 0 0 (-5)
        canvasPixels = 100
        black = mkColor 0 0 0
        color = mkColor 1 0 0
        material = mkMaterial & setColor (mkColor 1 0.2 1)
        shape =
            mkSphere
                & setMaterial material
        lightPosition = mkPoint (-10) 10 (-10)
        lightColor = mkColor 1 1 1
        light = mkPointLight lightPosition lightColor
        pixelSize = wallSize / fromIntegral canvasPixels
        half = wallSize / 2
        allCoords = [(x, y) | x <- [0 .. canvasPixels - 1], y <- [0 .. canvasPixels - 1]]
        op (x, y) =
            let worldY = half - pixelSize * fromIntegral y
                worldX = - half + pixelSize * fromIntegral x
                pos = mkPoint worldX worldY wallZ
                r = mkRay rayOrigin $ normalizeT $ subtractT pos rayOrigin
                xs = intersect r shape
             in case hit xs of
                    Just (Intersection tVal obj) ->
                        let point = position tVal r
                            normal = normalAt point obj
                            eye = negateT (direction r)
                            col = lighting (sphereMaterial obj) light point eye normal
                         in ((x, y), col)
                    Nothing -> ((x, y), black)
     in mkCanvasWithData canvasPixels canvasPixels ((op <$> allCoords) `using` parList rdeepseq)