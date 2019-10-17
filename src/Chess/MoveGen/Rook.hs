module Chess.MoveGen.Rook where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Common

potentialRookMoves             :: RegularBoardRepresentation -> Bitboard -> Player -> Coordinate -> [Move]
potentialRookMoves placement occupancy ply c = potentialRayMoves placement occupancy ply c straights where
  straights = [N, E, S, W]
