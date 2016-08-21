module Chess.MoveGen
  ( alongRay
  , isBlocked
  , pseudoLegalMoves
  , module Chess.MoveGen.Bishop
  , module Chess.MoveGen.King
  , module Chess.MoveGen.Knight
  , module Chess.MoveGen.Pawn
  , potentialQueenMoves
  , potentialRookMoves
  ) where

import Chess.Base

import Chess.MoveGen.Bishop
import Chess.MoveGen.King
import Chess.MoveGen.Knight
import Chess.MoveGen.Pawn
import Chess.MoveGen.Common

import Data.Maybe

import Control.Applicative

pseudoLegalMoves               :: RegularGame -> [Move]
pseudoLegalMoves game@Game { placement = b } = (concatMap . concatMap) (pseudoLegalMovesFrom game) b where

pseudoLegalMovesFrom :: RegularGame -> Square -> [Move]
pseudoLegalMovesFrom _ (Square Nothing _)            = []
pseudoLegalMovesFrom game (Square (Just (Piece p _)) l) | p == Pawn   = potentialPawnMoves game l
                                                        | p == Knight = potentialKnightMoves game l
                                                        | p == Bishop = potentialBishopMoves game l
                                                        | p == Rook   = potentialRookMoves game l
                                                        | p == Queen  = potentialQueenMoves game l
                                                        | p == King   = potentialKingMoves game l



potentialRookMoves     :: RegularGame -> Coordinate -> [Move]
potentialRookMoves Game { placement = b } c = filter (not . (isBlocked b)) $ potentialRayMoves b c straights where
  straights = [(1,0),(-1,0),(0,1),(0,-1)]

potentialQueenMoves     :: RegularGame -> Coordinate -> [Move]
potentialQueenMoves g c = potentialRookMoves g c ++ potentialBishopMoves g c
