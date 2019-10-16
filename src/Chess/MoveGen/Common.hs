module Chess.MoveGen.Common where

import Chess.Base
import Chess.Board

import Data.Maybe

data Ray = N | NE | E | SE | S | SW | W | NW

rayToOffsets :: Ray -> (Int, Int)
rayToOffsets N = (0, 1)
rayToOffsets NE = (1, 1)
rayToOffsets E = (1, 0)
rayToOffsets SE = (1, -1)
rayToOffsets S = (0, -1)
rayToOffsets SW = (-1, -1)
rayToOffsets W = (-1, 0)
rayToOffsets NW = (-1, 1)

potentialOffsetMoves             :: RegularBoardRepresentation -> Coordinate -> [(Int, Int)] -> [Move]
potentialOffsetMoves b c offsets = fmap (\x -> Move { moveFrom = c
                                                    , moveTo = x
                                                    , moveType = determineMoveType b c x 
                                                    , movePromoteTo = Nothing }) $ filter (flip (unoccupiedByAlly b) (fmap pieceOwner $ pieceAt b c)) $ filter isOnBoard $ fmap (c `offsetBy`) offsets

potentialRayMoves             :: RegularBoardRepresentation -> Coordinate -> [Ray] -> [Move]
potentialRayMoves b c ray = fmap (\x -> Move { moveFrom = c
                                                 , moveTo = x
                                                 , moveType = determineMoveType b c x
                                                 , movePromoteTo = Nothing }) $ filter isOnBoard $ fmap (c `offsetBy`) $ scaleBy <$> [1..7] <*> offsets
  where offsets = rayToOffsets <$> ray

determineMoveType                 :: RegularBoardRepresentation -> Coordinate -> Coordinate -> MoveType
determineMoveType b from to       | isNothing $ pieceAt b to                          = Standard
                                  | (fmap pieceOwner $ pieceAt b to) /= (fmap pieceOwner $ pieceAt b from) = Capture

isBlocked                       :: RegularBoardRepresentation -> Move -> Bool
isBlocked b Move { moveFrom = from
                 , moveTo = to }  = not $ to `elem` (validMoves $ alongRay (from, to)) where

  validMoves    :: [Coordinate] -> [Coordinate]
  validMoves cs = validMoves' cs False

  validMoves'                :: [Coordinate] -> Bool -> [Coordinate]
  validMoves' (c:cs) blocked | blocked == True = []
                             | otherwise       = case (fmap pieceOwner $ pieceAt b c) of
                                                   Nothing                -> c:validMoves' cs False
                                                   (Just owner) -> if ((Just owner) == (fmap pieceOwner $ pieceAt b from))
                                                                               then []
                                                                               else c:validMoves' cs True

alongRay            :: (Coordinate, Coordinate) -> [Coordinate]
alongRay (from, to) = filter (\x -> coordinateEuclideanDistance from x <= coordinateEuclideanDistance from to)
                    $ filter isOnBoard
                    $ fmap (from `offsetBy`)
                    $ scaleBy <$> [1..7] <*> [rayFromMove (from, to)]

rayFromMove                                    :: (Coordinate, Coordinate) -> (Int, Int)
rayFromMove (Coordinate f r, Coordinate f' r') | fromEnum f' > fromEnum f && r' > r = (1,1)
                                               | fromEnum f' > fromEnum f && r' < r = (1,-1)
                                               | fromEnum f' > fromEnum f && r' == r = (1,0)
                                               | fromEnum f' < fromEnum f && r' > r = (-1,1)
                                               | fromEnum f' < fromEnum f && r' < r = (-1,-1)
                                               | fromEnum f' < fromEnum f && r' == r = (-1,0)
                                               | fromEnum f' == fromEnum f && r' > r = (0,1)
                                               | fromEnum f' == fromEnum f && r' < r = (0,-1)
                                               | fromEnum f' == fromEnum f && r' == r = (0,0)

coordinateEuclideanDistance                                       :: Coordinate -> Coordinate -> Int
coordinateEuclideanDistance (Coordinate cx y) (Coordinate cx' y') = ((x' - x) ^ 2) + ((y' - y) ^ 2) where
  x' = fromEnum cx' - fromEnum 'a'
  x  = fromEnum cx - fromEnum 'a'

offsetBy                          :: Coordinate -> (Int, Int) -> Coordinate
offsetBy (Coordinate f r) (df,dr) = Coordinate (toEnum $ fromEnum f + df) (r + dr)

scaleBy                           :: Int -> (Int, Int) -> (Int, Int)
scaleBy s (x,y)                   = (x*s, y*s)

unoccupied     :: RegularBoardRepresentation -> Coordinate -> Bool
unoccupied b c = isNothing $ pieceAt b c

unoccupiedByAlly         :: RegularBoardRepresentation -> Coordinate -> Maybe Player -> Bool
unoccupiedByAlly b c ply | isNothing targetOwner = True
                         | ply /= targetOwner = True
                         | ply == targetOwner = False where
  targetPiece = pieceAt b c
  targetOwner = pieceOwner <$> targetPiece
