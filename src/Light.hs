module Light where

import Drawing.Color
import Tuple (T)

data Light = PointLight T Color
    deriving (Show, Eq)

mkPointLight :: T -> Color -> Light
mkPointLight = PointLight