module Chess.MoveGen.Bishop where

import Chess.Base

import Chess.MoveGen.Common

potentialBishopMoves     :: RegularGame -> Coordinate -> [Move]
potentialBishopMoves Game { placement = b } c = filter (not . (isBlocked b)) $ potentialRayMoves b c diagonals where
  diagonals = [(-1,1),(1,1),(1,-1),(-1,-1)]

