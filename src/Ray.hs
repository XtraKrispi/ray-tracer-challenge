module Ray where

import Intersections
import Matrix
import Object
import Tuple

data Ray = Ray {origin :: T, direction :: T}
    deriving (Show, Eq)

mkRay :: T -> T -> Ray
mkRay = Ray

position :: Double -> Ray -> T
position t (Ray o dir) = o `addT` (dir `multiplyScalar` t)

intersect :: Ray -> Object -> Intersections
intersect ray s@(Sphere sphereOrigin sphereTransform _) =
    let newRay = flip transform ray <$> inverse sphereTransform
        op (Ray rayOrigin rayDirection) =
            let sphereToRay = subtractT rayOrigin sphereOrigin
                a = dotProduct rayDirection rayDirection
                b = 2 * dotProduct rayDirection sphereToRay
                c = dotProduct sphereToRay sphereToRay - 1

                discriminant = (b ^ 2) - 4 * a * c
                t1 = (- b - sqrt discriminant) / (2 * a)
                t2 = (- b + sqrt discriminant) / (2 * a)
             in if discriminant < 0
                    then []
                    else [intersection t1 s, intersection t2 s]
     in maybe [] op newRay

transform :: Matrix -> Ray -> Ray
transform m (Ray o d) = Ray (multT m o) (multT m d)