module Object where

import Data.Function
import Data.Maybe
import Material
import Matrix
import Tuple

data Object = Sphere
    { sphereCentre :: T
    , sphereTransformation :: Matrix
    , sphereMaterial :: Material
    }
    deriving (Show, Eq)

mkSphere :: Object
mkSphere = Sphere (mkPoint 0 0 0) (identityMatrix 4 4) mkMaterial

setTransform :: Matrix -> Object -> Object
setTransform m o@Sphere{} = o{sphereTransformation = m}

setMaterial :: Material -> Object -> Object
setMaterial m o@Sphere{} = o{sphereMaterial = m}

normalAt :: T -> Object -> T
normalAt worldPoint (Sphere c t _) =
    let op invM =
            let objectPoint = invM `multT` worldPoint
                objectNormal = objectPoint `subtractT` mkPoint 0 0 0
                worldNormal = transposeM invM `multT` objectNormal
             in worldNormal & setW 0
     in normalizeT $ maybe worldPoint op (inverse t)