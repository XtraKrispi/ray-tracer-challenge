module Drawing.Canvas where

import Data.Map (Map)
import qualified Data.Map as Map
import Drawing.Color (Color)
import Utils (eq)

type Width = Int
type Height = Int

data Canvas = Canvas
    { canvasWidth :: Width
    , canvasHeight :: Height
    , canvasGrid :: Map (Int, Int) Color
    }
    deriving (Show)

mkCanvas :: Width -> Height -> Color -> Canvas
mkCanvas w h c = Canvas w h $ Map.fromList [((x, y), c) | x <- [0 .. w -1], y <- [0 .. h -1]]

toList :: Canvas -> [((Int, Int), Color)]
toList = Map.toList . canvasGrid

writePixel :: (Int, Int) -> Color -> Canvas -> Maybe Canvas
writePixel pos@(x, y) c canvas
    | x < canvasWidth canvas && y < canvasHeight canvas = Just $ canvas{canvasGrid = Map.insert pos c $ canvasGrid canvas}
    | otherwise = Nothing

pixelAt :: (Int, Int) -> Canvas -> Maybe Color
pixelAt pos@(x, y) c
    | x < canvasWidth c && y < canvasHeight c = Map.lookup pos (canvasGrid c)
    | otherwise = Nothing
