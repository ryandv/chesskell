module Chess.MoveGen.Bishop where

import Chess.Base

import Chess.MoveGen.Common

potentialBishopMoves             :: RegularBoardRepresentation -> Coordinate -> [Move]
potentialBishopMoves placement c = filter (not . (isBlocked placement)) $ potentialRayMoves placement c diagonals where
  diagonals = [NW, NE, SW, SE]
