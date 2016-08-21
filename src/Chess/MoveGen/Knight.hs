module Chess.MoveGen.Knight where

import Chess.Base

import Chess.MoveGen.Common

potentialKnightMoves     :: RegularGame -> Coordinate -> [Move]
potentialKnightMoves Game { placement = b } c = potentialOffsetMoves b c possibleJumps where
  possibleJumps = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]
