{-# LANGUAGE DeriveGeneric #-}

module Drawing.Color where

import Control.Parallel.Strategies
import GHC.Generics
import Utils (eq)

data Color = Color
    { colorR :: Double
    , colorG :: Double
    , colorB :: Double
    }
    deriving (Show, Generic)

instance NFData Color

instance Eq Color where
    (Color r g b) == (Color r' g' b') =
        (r `eq` r') && (g `eq` g') && (b `eq` b')

mkColor :: Double -> Double -> Double -> Color
mkColor = Color

op :: (Double -> Double -> Double) -> Color -> Color -> Color
op fn (Color r g b) (Color r' g' b') = Color (fn r r') (fn g g') (fn b b')

addC :: Color -> Color -> Color
addC = op (+)

subC :: Color -> Color -> Color
subC = op (-)

multC :: Color -> Color -> Color
multC = op (*)

multScalarC :: Color -> Double -> Color
multScalarC (Color r g b) n = Color (r * n) (g * n) (b * n)
