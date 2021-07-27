module Transformation where

import Matrix

translation :: Double -> Double -> Double -> Matrix
translation x y z = insert [((0, 3), x), ((1, 3), y), ((2, 3), z)] $ identityMatrix 4 4

scaling :: Double -> Double -> Double -> Matrix
scaling x y z = insert [((0, 0), x), ((1, 1), y), ((2, 2), z)] $ identityMatrix 4 4

rotationX :: Double -> Matrix
rotationX r = insert [((1, 1), cos r), ((1, 2), - sin r), ((2, 1), sin r), ((2, 2), cos r)] $ identityMatrix 4 4

rotationY :: Double -> Matrix
rotationY r = insert [((0, 0), cos r), ((0, 2), sin r), ((2, 0), - sin r), ((2, 2), cos r)] $ identityMatrix 4 4

rotationZ :: Double -> Matrix
rotationZ r = insert [((0, 0), cos r), ((0, 1), - sin r), ((1, 0), sin r), ((1, 1), cos r)] $ identityMatrix 4 4