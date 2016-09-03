module Chess.Predicates where

import Chess.Base
import Chess.Board
import Chess.MoveGen

import Control.Applicative

import Data.Maybe

moveIsLegal :: Move -> RegularGame -> Bool
moveIsLegal move@Move{ moveFrom = from} game = moveIsPseudoLegal && moveIsByRightPlayer && notCheckedAfterMove where

  moveIsPseudoLegal = (move `elem` pseudoLegalMoves game)
  moveIsByRightPlayer = (pieceOwner <$> (pieceOn $ (squareAt position from))) == (Just (activeColor game))
  notCheckedAfterMove = (not $ isChecked game { placement = positionAfterMove position move})
  position = placement game

noLegalMovesRemaining :: RegularGame -> Player -> Bool
noLegalMovesRemaining game ply = (null $ filter (\x -> pieceIsOwnedByPly x && (not $ isChecked game { placement = positionAfterMove (placement game) x })) $ pseudoLegalMoves game) where

  pieceIsOwnedByPly :: Move -> Bool
  pieceIsOwnedByPly Move { moveFrom = from } = (pieceOwner <$> (pieceOn $ (squareAt (placement game) from))) == (Just ply)

isCheckmate          :: RegularGame -> Player -> Bool
isCheckmate game ply = (isChecked game) && noLegalMovesRemaining game ply

isStalemate          :: RegularGame -> Player -> Bool
isStalemate game ply = (not $ isChecked game) && noLegalMovesRemaining game ply

-- Given an opponent's PieceType, this function works by placing down a ghost piece of the same PieceType but the active player's color,
-- and checks to see if the ghost piece can capture any of the opponent's pieces.
isChecking            :: RegularGame -> Coordinate -> PieceType -> (RegularGame -> [Move]) -> Bool
isChecking gameWithGhostPiece coord pt movegen = not
                                               $ null
                                               $ filter (liftA2 (&&) moveIsCapture moveMatchesPieceType)
                                               $ filter moveIsFromTargetCoordinate
                                               $ movegen gameWithGhostPiece where

  moveIsCapture :: Move -> Bool
  moveIsCapture = (== Capture) . moveType

  moveMatchesPieceType :: Move -> Bool
  moveMatchesPieceType = (== pt) . fromJust . (fmap pieceType) . pieceOn . squareAt (placement gameWithGhostPiece) . moveTo

  moveIsFromTargetCoordinate :: Move -> Bool
  moveIsFromTargetCoordinate = (== coord) . moveFrom

isAttacked :: RegularGame -> Coordinate -> Bool
isAttacked game coord = isQueenChecking || isRookChecking || isBishopChecking || isKnightChecking || isPawnChecking || isKingChecking where

  nextState = (placement game)

  activePly = (activeColor game)

  placeGhostPiece :: PieceType -> RegularGame
  placeGhostPiece pt = game { placement = addPiece nextState (Just (Piece pt activePly)) coord }

  isQueenChecking :: Bool
  isQueenChecking = isChecking (placeGhostPiece Queen) coord Queen pseudoLegalMoves

  isRookChecking :: Bool
  isRookChecking = isChecking (placeGhostPiece Rook) coord Rook pseudoLegalMoves

  isBishopChecking :: Bool
  isBishopChecking = isChecking (placeGhostPiece Bishop) coord Bishop pseudoLegalMoves

  isKnightChecking :: Bool
  isKnightChecking = isChecking (placeGhostPiece Knight) coord Knight pseudoLegalMoves

  -- TODO: do we need to consider en passant? I think not.
  isPawnChecking :: Bool
  isPawnChecking = isChecking (placeGhostPiece Pawn) coord Pawn pseudoLegalMoves

  -- TODO: do we need to consider castling? I think not.
  isKingChecking :: Bool
  isKingChecking = isChecking (placeGhostPiece King) coord King pseudoLegalMoves

isChecked      :: RegularGame -> Bool
isChecked game = isAttacked game (kingSquare (activeColor game)) where

  kingSquare     :: Player -> Coordinate
  kingSquare ply = location $ head $ filter ((== Just (Piece King ply)) . pieceOn) $ foldr (++) [] (placement game)
