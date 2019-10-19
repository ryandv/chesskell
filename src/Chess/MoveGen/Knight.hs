module Chess.MoveGen.Knight where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Common

potentialKnightMoves                      :: RegularBoardRepresentation -> BitboardRepresentation -> Coordinate -> [Move]
potentialKnightMoves placement bitboard c = potentialOffsetMoves placement bitboard c possibleJumps where
  possibleJumps = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]
