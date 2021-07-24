module Projectile where

import Data.Maybe (fromMaybe)
import Drawing.Canvas
import Drawing.Color
import Drawing.Output
import Tuple

data Environment = Environment {gravity :: T, wind :: T}
    deriving (Show)

data Projectile = Projectile {position :: T, velocity :: T}
    deriving (Show)

tick :: Environment -> Projectile -> Projectile
tick env proj =
    let newPos = position proj `addT` velocity proj
        newVel = velocity proj `addT` gravity env `addT` wind env
     in Projectile newPos newVel

simulate :: Environment -> Projectile -> Canvas -> IO ()
simulate env proj c = go 0 env proj c
  where
    go ticks env proj canvas = do
        if yVal (position proj) <= 0
            then do
                putStrLn $ "Hit the ground after " <> show ticks <> " ticks"
                writeRaw "projectile.ppm" $ getRawFile $ getOutput PPMType canvas
            else do
                let newProj = tick env proj
                    (x, y, _, _) = toTuple $ position newProj
                    newCanvas = writePixel (round x, canvasHeight canvas - round y) (mkColor 1 0 0) canvas
                putStrLn $ "New position: " <> show (position newProj)
                go (ticks + 1) env newProj $ fromMaybe canvas newCanvas

sampleStart = mkPoint 0 1 0
sampleVelocity = multiplyScalar (normalizeT $ mkVector 1 1.8 0) 11.25
sampleP = Projectile sampleStart sampleVelocity
sampleGravity = mkVector 0 (-0.1) 0
sampleWind = mkVector (-0.01) 0 0
sampleE = Environment sampleGravity sampleWind
sampleC = mkCanvas 900 550 (mkColor 0 0 0)

runSample = simulate sampleE sampleP sampleC