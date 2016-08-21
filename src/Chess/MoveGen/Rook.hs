module Chess.MoveGen.Rook where

import Chess.Base

import Chess.MoveGen.Common

potentialRookMoves     :: RegularGame -> Coordinate -> [Move]
potentialRookMoves Game { placement = b } c = filter (not . (isBlocked b)) $ potentialRayMoves b c straights where
  straights = [(1,0),(-1,0),(0,1),(0,-1)]
