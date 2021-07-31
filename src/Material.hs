module Material where

import Drawing.Color
import Light
import Tuple

data Material = Material
    { materialColor :: Color
    , materialAmbient :: Double
    , materialDiffuse :: Double
    , materialSpecular :: Double
    , materialShininess :: Double
    }
    deriving (Show, Eq)

mkMaterial :: Material
mkMaterial = Material (mkColor 1 1 1) 0.1 0.9 0.9 200

setColor :: Color -> Material -> Material
setColor c m = m{materialColor = c}

setAmbient :: Double -> Material -> Material
setAmbient a m = m{materialAmbient = a}

setDiffuse :: Double -> Material -> Material
setDiffuse d m = m{materialDiffuse = d}

setSpecular :: Double -> Material -> Material
setSpecular s m = m{materialSpecular = s}

setShininess :: Double -> Material -> Material
setShininess s m = m{materialShininess = s}

lighting :: Material -> Light -> T -> T -> T -> Color
lighting (Material c a d sp sh) (PointLight lp i) p e n =
    let effectiveColor = c `multC` i
        lightV = normalizeT $ lp `subtractT` p
        ambient = effectiveColor `multScalarC` a
        lightDotNormal = lightV `dotProduct` n
        reflectDotEye = reflect (negateT lightV) n `dotProduct` e
        factor = reflectDotEye ** sh
        diff = effectiveColor `multScalarC` (d * lightDotNormal)
        (diffuse, specular) =
            case (lightDotNormal, reflectDotEye) of
                (ldn, rde)
                    | ldn >= 0 && rde > 0 -> (diff, i `multScalarC` (sp * factor))
                    | ldn >= 0 && rde <= 0 -> (diff, black)
                    | otherwise -> (black, black)
     in ambient `addC` diffuse `addC` specular