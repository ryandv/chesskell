module Chess.MoveGen.Bishop where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Common

potentialBishopMoves                 :: RegularBoardRepresentation -> Bitboard -> Player -> Coordinate -> [Move]
potentialBishopMoves placement occupancy ply c = potentialRayMoves placement occupancy ply c diagonals where
  diagonals = [NW, NE, SW, SE]
