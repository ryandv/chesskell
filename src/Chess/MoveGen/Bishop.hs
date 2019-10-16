module Chess.MoveGen.Bishop where

import Chess.Base

import Chess.MoveGen.Common

potentialBishopMoves                 :: RegularBoardRepresentation -> Player -> Coordinate -> [Move]
potentialBishopMoves placement ply c = potentialRayMoves placement ply c diagonals where
  diagonals = [NW, NE, SW, SE]
