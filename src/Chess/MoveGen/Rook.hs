module Chess.MoveGen.Rook where

import Chess.Base

import Chess.MoveGen.Common

potentialRookMoves             :: RegularBoardRepresentation -> Coordinate -> [Move]
potentialRookMoves placement c = filter (not . (isBlocked placement)) $ potentialRayMoves placement c straights where
  straights = [N, E, S, W]
