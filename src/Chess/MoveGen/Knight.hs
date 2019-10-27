module Chess.MoveGen.Knight where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Common

potentialKnightMoves                :: BitboardRepresentation -> Player -> Coordinate -> [Move]
potentialKnightMoves bitboard ply c = potentialOffsetMoves possibleJumps bitboard ply c where
  possibleJumps = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]
