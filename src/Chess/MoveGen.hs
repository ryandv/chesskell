module Chess.MoveGen
  ( pseudoLegalMoves
  , potentialBishopMoves
  , potentialKingMoves
  , potentialKnightMoves
  , potentialPawnMoves
  , potentialRookMoves
  ) where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Common
import Chess.MoveGen.Bishop
import Chess.MoveGen.King
import Chess.MoveGen.Knight
import Chess.MoveGen.Pawn
import Chess.MoveGen.Rook

import Data.Maybe

import Control.Applicative

pseudoLegalMoves               :: Game BitboardRepresentation -> [Move]
pseudoLegalMoves game = concatMap (pseudoLegalMovesFrom (placement game) (castlingRights game) (enPassantSquare game)) $ occupiedCoordinates (placement game)

pseudoLegalMovesFrom                                                                         :: BitboardRepresentation -> CastleRights -> Maybe Coordinate -> Coordinate -> [Move]
pseudoLegalMovesFrom bitboards castlingRights enPassantSquare coord = pseudoLegalMovesFrom' (occupyingPiece coord)

  where pseudoLegalMovesFrom' Nothing = []
        pseudoLegalMovesFrom' (Just (Piece Pawn ply)) = potentialPawnMoves enPassantSquare bitboards ply coord
        pseudoLegalMovesFrom' (Just (Piece Knight ply)) = potentialKnightMoves bitboards ply coord
        pseudoLegalMovesFrom' (Just (Piece Bishop ply)) = bishopMoves ply
        pseudoLegalMovesFrom' (Just (Piece Rook ply)) = rookMoves ply
        pseudoLegalMovesFrom' (Just (Piece Queen ply)) = bishopMoves ply ++ rookMoves ply
        pseudoLegalMovesFrom' (Just (Piece King ply)) = potentialKingMoves castlingRights bitboards ply coord

        bishopMoves ply = potentialBishopMoves bitboards ply coord
        rookMoves ply   = potentialRookMoves bitboards ply coord
        occupancy       = totalOccupancy bitboards
        occupyingPiece  = bitboardPieceAt bitboards
