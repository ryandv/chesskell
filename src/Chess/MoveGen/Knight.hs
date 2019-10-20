module Chess.MoveGen.Knight where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Common

potentialKnightMoves            :: BitboardRepresentation -> Coordinate -> [Move]
potentialKnightMoves bitboard c = potentialOffsetMoves bitboard c possibleJumps where
  possibleJumps = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]
