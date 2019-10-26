module Chess.MoveGen.Rook where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Common

potentialRookMoves                :: BitboardRepresentation -> Player -> Coordinate -> [Move]
potentialRookMoves bitboard ply c = potentialRayMoves bitboard ply c straights where
  straights = [N, E, S, W]
