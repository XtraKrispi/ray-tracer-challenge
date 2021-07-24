module Utils where

import Data.Text (Text)
import qualified Data.Text as T

eq :: Double -> Double -> Bool
eq a b =
    let epsilon = 0.0001
     in abs (a - b) < 0.0001

tshow :: Show a => a -> Text
tshow = T.pack . show