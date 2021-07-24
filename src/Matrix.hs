module Matrix where

import Data.Array (Array, array, assocs, bounds, elems, indices, ixmap, (!), (//))
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Tuple (T (T), toTuple)

newtype Matrix = Matrix {asArray :: Array (Int, Int) Double}
    deriving (Show, Eq)

mkMatrix :: Int -> Int -> [((Int, Int), Double)] -> Matrix
mkMatrix rows cols d =
    let zeroed = [((row, col), 0) | col <- [0 .. cols - 1], row <- [0 .. rows - 1]]
     in Matrix $ array ((0, 0), (rows - 1, cols - 1)) $ zeroed `List.union` d

insert :: [((Int, Int), Double)] -> Matrix -> Matrix
insert d = Matrix . (// d) . asArray

identityMatrix :: Int -> Int -> Matrix
identityMatrix rows cols =
    let m = mkMatrix rows cols []
        updated = mapMaybe (\(row, col) -> if row == col then Just ((row, col), 1) else Nothing) $ indices (asArray m)
     in Matrix $ asArray m // updated

get :: (Int, Int) -> Matrix -> Maybe Double
get coord (Matrix m)
    | coord `elem` indices m = Just $ m ! coord
    | otherwise = Nothing

getAllInRow :: Int -> Array (Int, Int) Double -> [(Int, Int)]
getAllInRow row m =
    mapMaybe (\(coord@(r, _), _) -> if r == row then Just coord else Nothing) $ assocs m

getAllInCol :: Int -> Array (Int, Int) Double -> [(Int, Int)]
getAllInCol col m = mapMaybe (\(coord@(_, c), _) -> if c == col then Just coord else Nothing) $ assocs m

multM :: Matrix -> Matrix -> Matrix
multM (Matrix m1) (Matrix m2) =
    let getCell :: (Int, Int) -> Double
        getCell (row, col) =
            let allForRow = getAllInRow row m1
                allForCol = getAllInCol col m2
             in sum $ (\(m1Coord, m2Coord) -> (m1 ! m1Coord) * (m2 ! m2Coord)) <$> zip allForRow allForCol
        rowsInM = fst . snd . bounds
        colsInM = snd . snd . bounds
     in mkMatrix (rowsInM m1 + 1) (colsInM m2 + 1) $ (\coord -> (coord, getCell coord)) <$> [(row, col) | row <- [0 .. rowsInM m1], col <- [0 .. colsInM m2]]

multT :: Matrix -> T -> T
multT m t =
    let tAsMatrix = mkMatrix 4 1 $ (\(x, y, z, w) -> [((0, 0), x), ((1, 0), y), ((2, 0), z), ((3, 0), w)]) $ toTuple t
     in (\[x, y, z, w] -> T (x, y, z, w)) $ elems . asArray $ multM m tAsMatrix

transposeM :: Matrix -> Matrix
transposeM (Matrix m) =
    let (_, (rows, cols)) = bounds m
     in Matrix $ ixmap ((0, 0), (cols, rows)) (\(row, col) -> (col, row)) m