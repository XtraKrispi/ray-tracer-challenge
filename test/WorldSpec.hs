module WorldSpec where

import Data.Function
import Drawing.Color
import Intersections
import Light
import Material
import Object
import Ray
import Test.Hspec
import Transformation
import Tuple
import World

spec :: Spec
spec = describe "World" do
    it "Should create a world" do
        let w = mkWorld
        worldObjects w `shouldBe` []
        worldLight w `shouldBe` Nothing

    it "Should have the correct default world" do
        let light = mkPointLight (mkPoint (-10) 10 (-10)) (mkColor 1 1 1)
            s1 =
                mkSphere
                    & setMaterial
                        ( mkMaterial
                            & setColor (mkColor 0.8 1 0.6)
                            & setDiffuse 0.7
                            & setSpecular 0.2
                        )
            s2 =
                mkSphere
                    & setTransform (scaling 0.5 0.5 0.5)
            w = defaultWorld

        worldLight w `shouldBe` Just light
        worldObjects w `shouldBe` [s1, s2]

    it "Should intersect a world with a ray" do
        let w = defaultWorld
            r = mkRay (mkPoint 0 0 (-5)) (mkVector 0 0 1)
            xs = intersectWorld r w

        length xs `shouldBe` 4
        intersectionTValue <$> xs `shouldBe` [4, 4.5, 5.5, 6]