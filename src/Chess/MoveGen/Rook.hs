module Chess.MoveGen.Rook where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Common

potentialRookMoves                                    :: BitboardRepresentation -> Bitboard -> Player -> Coordinate -> [Move]
potentialRookMoves bitboard occupancy ply c = potentialRayMoves bitboard occupancy ply c straights where
  straights = [N, E, S, W]
