module Intersections where

import Data.Function (on)
import Data.List (find, sortBy)
import Data.Maybe (listToMaybe)
import Object

data Intersection = Intersection
    { intersectionTValue :: Double
    , intersectionObject :: Object
    }
    deriving (Show, Eq)

type Intersections = [Intersection]

intersection :: Double -> Object -> Intersection
intersection = Intersection

hit :: Intersections -> Maybe Intersection
hit =
    find ((>= 0) . intersectionTValue)
        . sortBy (on compare intersectionTValue)