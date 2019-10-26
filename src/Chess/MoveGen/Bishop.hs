module Chess.MoveGen.Bishop where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Common

potentialBishopMoves                 :: BitboardRepresentation -> Player -> Coordinate -> [Move]
potentialBishopMoves bitboard ply c = potentialRayMoves bitboard ply c diagonals where
  diagonals = [NW, NE, SW, SE]
