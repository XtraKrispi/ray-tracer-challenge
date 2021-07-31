module MaterialSpec where

import Drawing.Color
import Light
import Material
import Test.Hspec
import Tuple

spec :: Spec
spec =
    describe "Materials" do
        it "Should have default material values" do
            let m = mkMaterial

            materialColor m `shouldBe` mkColor 1 1 1
            materialAmbient m `shouldBe` 0.1
            materialDiffuse m `shouldBe` 0.9
            materialSpecular m `shouldBe` 0.9
            materialShininess m `shouldBe` 200
        describe "Lighting" do
            beforeAll (pure (mkMaterial, mkPoint 0 0 0)) do
                it "Should have correct lighting with the eye between the light and the surface" $ \(m, position) -> do
                    let eyeV = mkVector 0 0 (-1)
                        normalV = mkVector 0 0 (-1)
                        light = mkPointLight (mkPoint 0 0 (-10)) (mkColor 1 1 1)
                        result = lighting m light position eyeV normalV

                    result `shouldBe` mkColor 1.9 1.9 1.9

                it "Should have correct lighting with the eye opposite surface, light offset 45deg" $ \(m, position) -> do
                    let eyeV = mkVector 0 0 (-1)
                        normalV = mkVector 0 0 (-1)
                        light = mkPointLight (mkPoint 0 10 (-10)) (mkColor 1 1 1)
                        result = lighting m light position eyeV normalV

                    result `shouldBe` mkColor 0.7364 0.7364 0.7364

                it "Should have correct lighting with the eye in the path of the reflection vector" $ \(m, position) -> do
                    let eyeV = mkVector 0 (- sqrt 2 / 2) (- sqrt 2 / 2)
                        normalV = mkVector 0 0 (-1)
                        light = mkPointLight (mkPoint 0 10 (-10)) (mkColor 1 1 1)
                        result = lighting m light position eyeV normalV

                    result `shouldBe` mkColor 1.6364 1.6364 1.6364

                it "Should have correct lighting with the light behind the surface" $ \(m, position) -> do
                    let eyeV = mkVector 0 0 (-1)
                        normalV = mkVector 0 0 (-1)
                        light = mkPointLight (mkPoint 0 0 10) (mkColor 1 1 1)
                        result = lighting m light position eyeV normalV

                    result `shouldBe` mkColor 0.1 0.1 0.1