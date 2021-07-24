{-# LANGUAGE OverloadedStrings #-}

module Drawing.Output where

import Data.List (sort, sortBy)
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Drawing.Canvas
import Drawing.Color
import Utils

data FileType = PPMType

data OutputFile = PPMFile PPM

data PPMHeader = PPMHeader {ppmIdentifier :: Text, ppmWidth :: Int, ppmHeight :: Int, ppmMaxColorValue :: Int}

data PPM = PPM {header :: PPMHeader, pixelData :: [[(Int, Int, Int)]]}

convertToPPM :: Canvas -> PPM
convertToPPM (Canvas w h g) = PPM (PPMHeader "P3" w h 255) $ Split.chunksOf w (convertColorToTuple . snd <$> sortBy (\a b -> compare (fst a) (fst b)) (Map.toList g))
 where
  convertColorToTuple (Color r g b) = (op r, op g, op b)
  op = normalize . round . (* 255)
  normalize :: Int -> Int
  normalize i
    | i < 0 = 0
    | i > 255 = 255
    | otherwise = i

zipToSpace :: (Text, Char, Text) -> [Text]
zipToSpace (l, n, rs) | T.null l = [T.cons n rs]
zipToSpace (l, n, r)
  | n == ' ' = [T.reverse l, r]
  | otherwise = zipToSpace (T.tail l, T.head l, T.cons n r)

convertPPMToRaw :: PPM -> [Text]
convertPPMToRaw (PPM header ps) = writeHeader header <> writePixels ps <> ["\n"]
 where
  writeHeader (PPMHeader i w h m) = [i, tshow w <> " " <> tshow h, tshow m]

  writePixels :: [[(Int, Int, Int)]] -> [Text]
  writePixels ps = ps >>= singleLine

  singleLine :: [(Int, Int, Int)] -> [Text]
  singleLine l = check $ T.splitAt 70 wholeLine
   where
    wholeLine = T.intercalate " " wholeLineSpaced
    check (a, b)
      | T.null b = [a]
      | T.head b == ' ' = [a, T.tail b]
      | otherwise = zipToSpace (T.reverse a, T.head b, T.tail b)

    wholeLineSpaced = foldr (\(r, g, b) t -> tshow r : tshow g : tshow b : t) [] l

getOutput :: FileType -> Canvas -> OutputFile
getOutput PPMType = PPMFile . convertToPPM

getRawFile :: OutputFile -> [Text]
getRawFile (PPMFile ppm) = convertPPMToRaw ppm

writeRaw :: FilePath -> [Text] -> IO ()
writeRaw fp = TIO.writeFile fp . T.unlines