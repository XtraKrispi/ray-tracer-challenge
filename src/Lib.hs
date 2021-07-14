{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where

newtype MyNum = MyNum { unNum :: Double }
    deriving (Show, Num, Fractional, Floating)

instance Eq MyNum where
    (MyNum a) == (MyNum b) = abs (a - b) < 0.0001

data T = Vector MyNum MyNum MyNum
        | Point MyNum MyNum MyNum
    deriving (Show, Eq)

mkVector :: Double -> Double -> Double -> T
mkVector x y z = Vector (MyNum x) (MyNum y) (MyNum z)

mkPoint :: Double -> Double -> Double -> T
mkPoint x y z = Point (MyNum x) (MyNum y) (MyNum z)

toTuple :: T -> (MyNum, MyNum, MyNum, MyNum)
toTuple (Vector x y z) = (x,y,z,0)
toTuple (Point x y z)  = (x,y,z,1)

addT :: T -> T -> Maybe T
addT (Vector x y z) (Vector x' y' z') = Just $ Vector (x + x') (y + y') (z + z')
addT (Vector x y z) (Point x' y' z')  = Just $ Point (x+x') (y+y') (z+z')
addT (Point x y z) (Vector x' y' z')  = Just $ Point (x+x') (y+y') (z+z')
addT _ _                              = Nothing

subtractT :: T -> T -> Maybe T
subtractT (Point x y z) (Point x' y' z')= Just $ Vector (x - x') (y - y') (z - z')
subtractT (Point x y z) (Vector x' y' z') = Just $ Point (x - x') (y - y') (z - z')
subtractT (Vector x y z) (Vector x' y' z') = Just $ Vector (x - x') (y - y') (z - z')
subtractT _ _ = Nothing

negateT :: T -> T
negateT (Point x y z)  = Point (-x) (-y) (-z)
negateT (Vector x y z) = Vector (-x) (-y) (-z)

multiplyT :: T -> Double -> T
multiplyT (Point x y z)  n = Point (x*MyNum n) (y*MyNum n) (z*MyNum n)
multiplyT (Vector x y z) n = Vector (x*MyNum n) (y*MyNum n) (z*MyNum n)

divideT :: T -> Double -> Maybe T
divideT _              0 = Nothing
divideT (Point x y z)  n = Just $ Point (x / MyNum n) (y / MyNum n) (z / MyNum n)
divideT (Vector x y z) n  = Just $ Vector (x / MyNum n) (y / MyNum n) (z / MyNum n)

magnitudeT :: T -> Maybe MyNum
magnitudeT (Vector x y z) = Just $ sqrt ((x ^ 2) + (y ^ 2) + (z ^ 2))
magnitudeT _              = Nothing
