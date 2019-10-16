module Chess.MoveGen.Bishop where

import Chess.Base

import Chess.MoveGen.Common

potentialBishopMoves             :: RegularBoardRepresentation -> Coordinate -> [Move]
potentialBishopMoves placement c = potentialRayMoves placement c diagonals where
  diagonals = [NW, NE, SW, SE]
