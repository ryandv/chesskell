module Chess.MoveGen.Common where

import Chess.Base
import Chess.Board
import Chess.Bitboard

import Data.Maybe

data Ray = N | NE | E | SE | S | SW | W | NW deriving(Eq, Show)

rayToOffsets :: Ray -> (Int, Int)
rayToOffsets N = (0, 1)
rayToOffsets NE = (1, 1)
rayToOffsets E = (1, 0)
rayToOffsets SE = (1, -1)
rayToOffsets S = (0, -1)
rayToOffsets SW = (-1, -1)
rayToOffsets W = (-1, 0)
rayToOffsets NW = (-1, 1)

rayGeneratorFor :: Ray -> (Int, Int) -> Bitboard
rayGeneratorFor N = northRay
rayGeneratorFor NE = northEastRay
rayGeneratorFor E = eastRay
rayGeneratorFor SE = southEastRay
rayGeneratorFor S = southRay
rayGeneratorFor SW = southWestRay
rayGeneratorFor W = westRay
rayGeneratorFor NW = northWestRay

liftOp :: (a -> b -> c) -> b -> [a] -> [c]
liftOp f c xs = (flip f) c <$> xs

potentialOffsetMoves             :: RegularBoardRepresentation -> Coordinate -> [(Int, Int)] -> [Move]
potentialOffsetMoves b c offsets = fmap (\x -> Move { moveFrom = c
                                                    , moveTo = x
                                                    , moveType = determineMoveType b c x
                                                    , movePromoteTo = Nothing }) $ filter (flip (unoccupiedByAlly b) (fmap pieceOwner $ pieceAt b c)) $ filter isOnBoard $ fmap (c `offsetBy`) offsets

potentialRayMoves          :: RegularBoardRepresentation -> Coordinate -> [Ray] -> [Move]
potentialRayMoves b c rays = concat $ potentialRayMoves' b c <$> rays

potentialRayMoves' :: RegularBoardRepresentation -> Coordinate -> Ray -> [Move]
potentialRayMoves' b c r | r == E || r == N || r == NE || r == NW = potentialPositiveRayMoves b c r
                         | otherwise = potentialNegativeRayMoves b c r

potentialPositiveRayMoves :: RegularBoardRepresentation -> Coordinate -> Ray -> [Move]
potentialPositiveRayMoves b c r = filter (not . (isBlocked b))
  $ fmap destinationToMove
  . bitboardToCoordinates
  $ rayGeneratorFor r (coordinateToIndices c)
    where destinationToMove dest = Move { moveFrom = c
                                        , moveTo = dest
                                        , moveType = determineMoveType b c dest
                                        , movePromoteTo = Nothing }

potentialNegativeRayMoves :: RegularBoardRepresentation -> Coordinate -> Ray -> [Move]
potentialNegativeRayMoves b c r = filter (not . (isBlocked b))
  $ fmap destinationToMove
  . bitboardToCoordinates
  $ rayGeneratorFor r (coordinateToIndices c)
    where destinationToMove dest = Move { moveFrom = c
                                        , moveTo = dest
                                        , moveType = determineMoveType b c dest
                                        , movePromoteTo = Nothing }

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
