module Chess.MoveGen.Knight where

import Chess.Base

import Chess.MoveGen.Common

potentialKnightMoves             :: RegularBoardRepresentation -> Coordinate -> [Move]
potentialKnightMoves placement c = potentialOffsetMoves placement c possibleJumps where
  possibleJumps = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]
