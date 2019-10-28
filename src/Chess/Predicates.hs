module Chess.Predicates
  ( isAttacked
  , isChecked
  , isCheckmate
  , isStalemate
  , moveIsLegal
  ) where

import Chess.Base
import Chess.Board
import Chess.Bitboard
import Chess.MoveGen

import Control.Applicative

import Data.Maybe

-- Given an opponent's PieceType, this function works by placing down a ghost piece of the same PieceType but the active player's color,
-- and checks to see if the ghost piece can capture any of the opponent's pieces.
isAttacked :: Game BitboardRepresentation -> Coordinate -> Bool
isAttacked game coord = isQueenChecking || isRookChecking || isBishopChecking || isKnightChecking || isPawnChecking || isKingChecking where
  bitboards = (placement game)

  rookAttacks = (potentialRookMoves bitboards activePly coord)
  bishopAttacks = (potentialBishopMoves bitboards activePly coord)

  activePly = (activeColor game)

  isQueenChecking :: Bool
  isQueenChecking = fastIsChecking (rookAttacks ++ bishopAttacks) bitboards coord Queen

  isRookChecking :: Bool
  isRookChecking = fastIsChecking rookAttacks bitboards coord Rook

  isBishopChecking :: Bool
  isBishopChecking = fastIsChecking bishopAttacks bitboards coord Bishop

  isKnightChecking :: Bool
  isKnightChecking = fastIsChecking (potentialKnightMoves bitboards activePly coord) bitboards coord Knight

  -- TODO: do we need to consider en passant? I think not.
  isPawnChecking :: Bool
  isPawnChecking = fastIsChecking (potentialPawnMoves (enPassantSquare game) bitboards activePly coord) bitboards coord Pawn

  -- TODO: do we need to consider castling? I think not.
  isKingChecking :: Bool
  isKingChecking = fastIsChecking (potentialKingMoves (castlingRights game) bitboards activePly coord) bitboards coord King

isChecked      :: Game BitboardRepresentation -> Bool
isChecked game = isAttacked game (kingSquare (activeColor game)) where

  kingSquare     :: Player -> Coordinate
  kingSquare White = indicesToCoordinate . squareIndexToIndices $ bitscanForward (whiteKings $ placement game)
  kingSquare Black = indicesToCoordinate . squareIndexToIndices $ bitscanForward (blackKings $ placement game)

isCheckmate          :: RegularGame -> Player -> Bool
isCheckmate game ply = (isChecked bitboardGame) && noLegalMovesRemaining game ply
  where bitboardGame = Game {
      placement = regularToBitboard $ placement game
    , activeColor = activeColor game
    , castlingRights = castlingRights game
    , enPassantSquare = enPassantSquare game
    , halfMoveClock = halfMoveClock game
    , fullMoveNumber = fullMoveNumber game
    }

isStalemate          :: RegularGame -> Player -> Bool
isStalemate game ply = (not $ isChecked bitboardGame) && noLegalMovesRemaining game ply
  where bitboardGame = Game {
      placement = regularToBitboard $ placement game
    , activeColor = activeColor game
    , castlingRights = castlingRights game
    , enPassantSquare = enPassantSquare game
    , halfMoveClock = halfMoveClock game
    , fullMoveNumber = fullMoveNumber game
    }

moveIsLegal :: Move -> RegularGame -> Bool
moveIsLegal move@Move{ moveFrom = from } game = moveIsPseudoLegal && moveIsByRightPlayer && (notCheckedAfterMove game move) where

  moveIsPseudoLegal = (move `elem` pseudoLegalMoves game)
  moveIsByRightPlayer = (pieceOwner <$> (pieceAt position from)) == (Just (activeColor game))
  position = placement game

notCheckedAfterMove :: RegularGame -> Move -> Bool
notCheckedAfterMove game move = (not $ isChecked bitboardGame { placement = regularToBitboard $ positionAfterMove (placement game) move})
  where bitboardGame = Game {
      placement = regularToBitboard $ placement game
    , activeColor = activeColor game
    , castlingRights = castlingRights game
    , enPassantSquare = enPassantSquare game
    , halfMoveClock = halfMoveClock game
    , fullMoveNumber = fullMoveNumber game
    }

noLegalMovesRemaining :: RegularGame -> Player -> Bool
noLegalMovesRemaining game ply = null
                               $ filter (liftA2 (&&) pieceIsOwnedByPly (notCheckedAfterMove game))
                               $ pseudoLegalMoves game where

  pieceIsOwnedByPly :: Move -> Bool
  pieceIsOwnedByPly Move { moveFrom = from } = (pieceOwner <$> (pieceAt (placement game) from)) == (Just ply)

fastIsChecking :: [Move] -> BitboardRepresentation -> Coordinate -> PieceType -> Bool
fastIsChecking moves bitboard coord pt = not
                                       . null
                                       . filter (liftA2 (&&) moveIsCapture (moveMatchesPieceType pt))
                                       . filter moveIsFromTargetCoordinate
                                       $ moves

  where moveIsCapture :: Move -> Bool
        moveIsCapture = (== Capture) . moveType

        moveMatchesPieceType    :: PieceType -> Move -> Bool
        moveMatchesPieceType pt = (== (Just pt)) . (fmap pieceType) . bitboardPieceAt bitboard . moveTo

        moveIsFromTargetCoordinate :: Move -> Bool
        moveIsFromTargetCoordinate = (== coord) . moveFrom

isChecking                             :: RegularGame -> Coordinate -> PieceType -> Bool
isChecking gameWithGhostPiece coord pt = not
                                       $ null
                                       $ filter (liftA2 (&&) moveIsCapture moveMatchesPieceType)
                                       $ filter moveIsFromTargetCoordinate
                                       $ pseudoLegalMoves gameWithGhostPiece where

  moveIsCapture :: Move -> Bool
  moveIsCapture = (== Capture) . moveType

  moveMatchesPieceType :: Move -> Bool
  moveMatchesPieceType = (== pt) . fromJust . (fmap pieceType) . pieceAt (placement gameWithGhostPiece) . moveTo

  moveIsFromTargetCoordinate :: Move -> Bool
  moveIsFromTargetCoordinate = (== coord) . moveFrom
