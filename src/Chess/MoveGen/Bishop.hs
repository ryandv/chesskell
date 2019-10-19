module Chess.MoveGen.Bishop where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Common

potentialBishopMoves                 :: BitboardRepresentation -> Bitboard -> Player -> Coordinate -> [Move]
potentialBishopMoves bitboard occupancy ply c = potentialRayMoves bitboard occupancy ply c diagonals where
  diagonals = [NW, NE, SW, SE]
