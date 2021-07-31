module World where

import Control.Monad
import Data.Function
import Data.List (sortBy)
import Drawing.Color
import Intersections
import Light
import Material
import Object
import Ray
import Transformation
import Tuple

data World = World {worldObjects :: [Object], worldLight :: Maybe Light}
    deriving (Eq, Show)

mkWorld :: World
mkWorld = World [] Nothing

defaultWorld :: World
defaultWorld =
    World
        [ mkSphere
            & setMaterial
                ( mkMaterial
                    & setColor (mkColor 0.8 1.0 0.6)
                    & setDiffuse 0.7
                    & setSpecular 0.2
                )
        , mkSphere & setTransform (scaling 0.5 0.5 0.5)
        ]
        (Just (mkPointLight (mkPoint (-10) 10 (-10)) (mkColor 1 1 1)))

intersectWorld :: Ray -> World -> Intersections
intersectWorld ray = sortBy (on compare intersectionTValue) . (intersect ray <=< worldObjects)