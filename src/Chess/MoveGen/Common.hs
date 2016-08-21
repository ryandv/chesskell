module Chess.MoveGen.Common where

import Chess.Base

import Data.Maybe

potentialOffsetMoves             :: RegularBoardRepresentation -> Coordinate -> [(Int, Int)] -> [Move]
potentialOffsetMoves b c offsets = fmap (\x -> Move { moveFrom = c
                                                    , moveTo = x
                                                    , moveType = determineMoveType b c x 
                                                    , movePromoteTo = Nothing }) $ filter (flip (unoccupiedByAlly b) (fmap pieceOwner $ pieceOn $ squareAt b c)) $ filter isOnBoard $ fmap (c `offsetBy`) offsets

potentialRayMoves             :: RegularBoardRepresentation -> Coordinate -> [(Int, Int)] -> [Move]
potentialRayMoves b c offsets = fmap (\x -> Move { moveFrom = c
                                                 , moveTo = x
                                                 , moveType = determineMoveType b c x
                                                 , movePromoteTo = Nothing }) $ filter isOnBoard $ fmap (c `offsetBy`) $ scaleBy <$> [1..7] <*> offsets where

determineMoveType                 :: RegularBoardRepresentation -> Coordinate -> Coordinate -> MoveType
determineMoveType b from to       | isNothing . pieceOn $ squareAt b to                          = Standard
                                  | (fmap pieceOwner . pieceOn $ squareAt b to) /= (fmap pieceOwner . pieceOn $ squareAt b from) = Capture

isBlocked                       :: RegularBoardRepresentation -> Move -> Bool
isBlocked b Move { moveFrom = from
                 , moveTo = to }  = not $ to `elem` (validMoves $ alongRay (from, to)) where

  validMoves    :: [Coordinate] -> [Coordinate]
  validMoves cs = validMoves' cs False

  validMoves'                :: [Coordinate] -> Bool -> [Coordinate]
  validMoves' (c:cs) blocked | blocked == True = []
                             | otherwise       = case (fmap pieceOwner $ pieceOn $ squareAt b c) of
                                                   Nothing                -> c:validMoves' cs False
                                                   (Just owner) -> if ((Just owner) == (fmap pieceOwner $ pieceOn $ squareAt b from))
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
