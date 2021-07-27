module Object where

import Tuple

data Object = Sphere {sphereCentre :: T}
    deriving (Show, Eq)

mkSphere :: Object
mkSphere = Sphere (mkPoint 0 0 0)