module Matrix where

import Data.Array (Array, array, assocs, bounds, elems, indices, ixmap, listArray, (!), (//))
import qualified Data.List as List
import Data.Maybe (fromMaybe, mapMaybe)
import Tuple (T (T), toTuple)
import Utils

newtype Matrix = Matrix {asArray :: Array (Int, Int) Double}
    deriving (Show)

instance Eq Matrix where
    (Matrix m1) == (Matrix m2) = all (\(((r1, c1), v1), ((r2, c2), v2)) -> r1 == r2 && c1 == c2 && v1 `eq` v2) $ zip (assocs m1) (assocs m2)

rows :: Matrix -> Int
rows (Matrix m) =
    let (_, (rows, cols)) = bounds m
     in rows + 1

cols :: Matrix -> Int
cols (Matrix m) =
    let (_, (rows, cols)) = bounds m
     in cols + 1

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

getAllInRow :: Int -> Array (Int, Int) Double -> [((Int, Int), Double)]
getAllInRow row m =
    mapMaybe (\a@((r, _), _) -> if r == row then Just a else Nothing) $ assocs m

getAllInCol :: Int -> Array (Int, Int) Double -> [((Int, Int), Double)]
getAllInCol col m = mapMaybe (\a@((_, c), _) -> if c == col then Just a else Nothing) $ assocs m

multM :: Matrix -> Matrix -> Matrix
multM (Matrix m1) (Matrix m2) =
    let getCell :: (Int, Int) -> Double
        getCell (row, col) =
            let allForRow = fst <$> getAllInRow row m1
                allForCol = fst <$> getAllInCol col m2
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

determinant :: Matrix -> Double
determinant m@(Matrix a)
    | rows m > 2 && cols m > 2 =
        let row = (\((r, c), v) -> v * cofactor r c m) <$> getAllInRow 0 a
         in sum row
    | otherwise =
        let a = (0, 0) `get` m
            b = (0, 1) `get` m
            c = (1, 0) `get` m
            d = (1, 1) `get` m
         in fromMaybe 0 $ (\a' b' c' d' -> a' * d' - b' * c') <$> a <*> b <*> c <*> d

submatrix :: Int -> Int -> Matrix -> Matrix
submatrix row col (Matrix m) =
    let allElems = assocs m
        (_, (rows, cols)) = bounds m
        filtered = snd <$> filter (\((r, c), _) -> r /= row && c /= col) allElems
     in Matrix $ listArray ((0, 0), (rows - 1, cols - 1)) filtered

minor :: Int -> Int -> Matrix -> Double
minor row col = determinant . submatrix row col

cofactor :: Int -> Int -> Matrix -> Double
cofactor row col m
    | even (row + col) = minor row col m
    | otherwise = - minor row col m

data Invertibility = Invertible | NotInvertible
    deriving (Show, Eq)

invertible :: Matrix -> Invertibility
invertible m
    | determinant m == 0 = NotInvertible
    | otherwise = Invertible

inverse :: Matrix -> Maybe Matrix
inverse m@(Matrix _) | invertible m == NotInvertible = Nothing
inverse m@(Matrix a) =
    Just $ mkMatrix (rows m) (cols m) $ (\((row, col), v) -> ((col, row), cofactor row col m / determinant m)) <$> assocs a
