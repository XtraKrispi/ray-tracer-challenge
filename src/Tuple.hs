{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tuple where

import Utils (eq)

newtype T = T {unT :: (Double, Double, Double, Double)}
    deriving (Show)

instance Eq T where
    (T (x, y, z, w)) == (T (x', y', z', w')) =
        (x `eq` x') && (y `eq` y') && (z `eq` z') && (w `eq` w')

mkVector :: Double -> Double -> Double -> T
mkVector x y z = T (x, y, z, 0)

mkPoint :: Double -> Double -> Double -> T
mkPoint x y z = T (x, y, z, 1)

toTuple :: T -> (Double, Double, Double, Double)
toTuple = unT

setW :: Double -> T -> T
setW w (T (x, y, z, _)) = T (x, y, z, w)

xVal :: T -> Double
xVal (T (x, _, _, _)) = x

yVal :: T -> Double
yVal (T (_, y, _, _)) = y

zVal :: T -> Double
zVal (T (_, _, z, _)) = z

wVal :: T -> Double
wVal (T (_, _, _, w)) = w

addT :: T -> T -> T
addT (T (x, y, z, w)) (T (x', y', z', w')) = T (x + x', y + y', z + z', w + w')

subtractT :: T -> T -> T
subtractT (T (x, y, z, w)) (T (x', y', z', w')) = T (x - x', y - y', z - z', w - w')

negateT :: T -> T
negateT (T (x, y, z, w)) = T (- x, - y, - z, - w)

multiplyScalar :: T -> Double -> T
multiplyScalar (T (x, y, z, w)) n = T (x * n, y * n, z * n, w * n)

divideScalar :: T -> Double -> Maybe T
divideScalar _ 0 = Nothing
divideScalar (T (x, y, z, w)) n = Just $ T (x / n, y / n, z / n, w / n)

magnitudeT :: T -> Double
magnitudeT (T (x, y, z, w)) = sqrt (x ^ 2 + y ^ 2 + z ^ 2 + w ^ 2)

normalizeT :: T -> T
normalizeT t@(T (x, y, z, w)) = T (x / magnitudeT t, y / magnitudeT t, z / magnitudeT t, w / magnitudeT t)

dotProduct :: T -> T -> Double
dotProduct (T (x, y, z, w)) (T (x', y', z', w')) = x * x' + y * y' + z * z' + w * w'

crossProduct :: T -> T -> T
crossProduct (T (x, y, z, w)) (T (x', y', z', w')) =
    mkVector (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

reflect :: T -> T -> T
reflect v n = v `subtractT` (n `multiplyScalar` (2 * dotProduct v n))
