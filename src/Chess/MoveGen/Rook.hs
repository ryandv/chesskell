module Chess.MoveGen.Rook where

import Chess.Base

import Chess.MoveGen.Common

potentialRookMoves             :: RegularBoardRepresentation -> Player -> Coordinate -> [Move]
potentialRookMoves placement ply c = potentialRayMoves placement ply c straights where
  straights = [N, E, S, W]
