module Chess.MoveGen
  ( alongRay
  , isBlocked
  , pseudoLegalMoves
  ) where

import Chess.Base

import Chess.MoveGen.Common
import Chess.MoveGen.Bishop
import Chess.MoveGen.King
import Chess.MoveGen.Knight
import Chess.MoveGen.Pawn
import Chess.MoveGen.Queen
import Chess.MoveGen.Rook

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
