module Chess.Predicates
  ( isAttacked
  , isChecked
  , isCheckmate
  , isCastleSafe
  , isStalemate
  , moveIsLegal
  ) where

import           Chess.Base
import           Chess.Board
import           Chess.Bitboard
import           Chess.MoveGen

import           Control.Applicative

import           Data.Maybe

-- Given an opponent's PieceType, this function works by placing down a ghost piece of the same PieceType but the active player's color,
-- and checks to see if the ghost piece can capture any of the opponent's pieces.
isAttacked :: Game BitboardRepresentation -> Coordinate -> Bool
isAttacked game coord =
  isQueenChecking
    || isRookChecking
    || isBishopChecking
    || isKnightChecking
    || isPawnChecking
    || isKingChecking where
  bitboards     = (placement game)

  rookAttacks   = (potentialRookMoves bitboards activePly coord)
  bishopAttacks = (potentialBishopMoves bitboards activePly coord)

  activePly     = (activeColor game)

  isQueenChecking :: Bool
  isQueenChecking =
    fastIsChecking (rookAttacks ++ bishopAttacks) bitboards coord Queen

  isRookChecking :: Bool
  isRookChecking = fastIsChecking rookAttacks bitboards coord Rook

  isBishopChecking :: Bool
  isBishopChecking = fastIsChecking bishopAttacks bitboards coord Bishop

  isKnightChecking :: Bool
  isKnightChecking = fastIsChecking
    (potentialKnightMoves bitboards activePly coord)
    bitboards
    coord
    Knight

  -- TODO: do we need to consider en passant? I think not.
  isPawnChecking :: Bool
  isPawnChecking = fastIsChecking
    (potentialPawnMoves (enPassantSquare game) bitboards activePly coord)
    bitboards
    coord
    Pawn

  -- TODO: do we need to consider castling? I think not.
  isKingChecking :: Bool
  isKingChecking = fastIsChecking
    (potentialKingMoves (castlingRights game) bitboards activePly coord)
    bitboards
    coord
    King

isChecked :: Game BitboardRepresentation -> Bool
isChecked game = isAttacked game (kingSquare (activeColor game)) where

  kingSquare :: Player -> Coordinate
  kingSquare White =
    indicesToCoordinate . squareIndexToIndices $ bitscanForward
      (whiteKings $ placement game)
  kingSquare Black =
    indicesToCoordinate . squareIndexToIndices $ bitscanForward
      (blackKings $ placement game)

isCheckmate :: Game BitboardRepresentation -> Player -> Bool
isCheckmate game ply = (isChecked game) && noLegalMovesRemaining game ply

isStalemate :: Game BitboardRepresentation -> Player -> Bool
isStalemate game ply = (not $ isChecked game) && noLegalMovesRemaining game ply

moveIsLegal :: Move -> Game BitboardRepresentation -> Bool
moveIsLegal move@(Move from to) game =
  moveIsPseudoLegal game move && moveIsByRightPlayer game move && notCheckedAfterMove game move
moveIsLegal move@(Capture from to) game =
  moveIsPseudoLegal game move && moveIsByRightPlayer game move && notCheckedAfterMove game move
moveIsLegal move@(Castle from to) game =
  moveIsPseudoLegal game move && moveIsByRightPlayer game move && notCheckedAfterMove game move
moveIsLegal move@(EnPassant from to) game =
  moveIsPseudoLegal game move && moveIsByRightPlayer game move && notCheckedAfterMove game move
moveIsLegal move@(Promote from to _) game =
  moveIsPseudoLegal game move && moveIsByRightPlayer game move && notCheckedAfterMove game move where

moveIsPseudoLegal :: Game BitboardRepresentation -> Move -> Bool
moveIsPseudoLegal game move = (move `elem` pseudoLegalMoves game)

moveIsByRightPlayer :: Game BitboardRepresentation -> Move -> Bool
moveIsByRightPlayer game (Move from _) = (pieceOwner <$> (bitboardPieceAt (placement game) from))
  == (Just (activeColor game))
moveIsByRightPlayer game (Capture from _) = (pieceOwner <$> (bitboardPieceAt (placement game) from))
  == (Just (activeColor game))
moveIsByRightPlayer game (EnPassant from _) = (pieceOwner <$> (bitboardPieceAt (placement game) from))
  == (Just (activeColor game))
moveIsByRightPlayer game (Castle from _) = (pieceOwner <$> (bitboardPieceAt (placement game) from))
  == (Just (activeColor game))
moveIsByRightPlayer game (Promote from _ _) = (pieceOwner <$> (bitboardPieceAt (placement game) from))
  == (Just (activeColor game))

notCheckedAfterMove :: Game BitboardRepresentation -> Move -> Bool
notCheckedAfterMove game move =
  (not $ isChecked game { placement = bitboardMovePiece (placement game) move })

noLegalMovesRemaining :: Game BitboardRepresentation -> Player -> Bool
noLegalMovesRemaining game ply =
  null
    $ filter validCastles
    $ filter (liftA2 (&&) pieceIsOwnedByPly (notCheckedAfterMove game))
    $ pseudoLegalMoves game where

  pieceIsOwnedByPly :: Move -> Bool
  pieceIsOwnedByPly (Move from to) =
    (pieceOwner <$> (bitboardPieceAt (placement game) from)) == (Just ply)
  pieceIsOwnedByPly (Capture from to) =
    (pieceOwner <$> (bitboardPieceAt (placement game) from)) == (Just ply)
  pieceIsOwnedByPly (EnPassant from to) =
    (pieceOwner <$> (bitboardPieceAt (placement game) from)) == (Just ply)
  pieceIsOwnedByPly (Castle from to) =
    (pieceOwner <$> (bitboardPieceAt (placement game) from)) == (Just ply)
  pieceIsOwnedByPly (Promote from to _) =
    (pieceOwner <$> (bitboardPieceAt (placement game) from)) == (Just ply)

  validCastles :: Move -> Bool
  validCastles (Castle from to)
    | from == Coordinate 'e' 1 && to == Coordinate 'g' 1 = isCastleSafe
      Kingside
      game
      White
    | from == Coordinate 'e' 8 && to == Coordinate 'g' 8 = isCastleSafe
      Kingside
      game
      Black
    | from == Coordinate 'e' 1 && to == Coordinate 'c' 1 = isCastleSafe
      Queenside
      game
      White
    | from == Coordinate 'e' 8 && to == Coordinate 'c' 8 = isCastleSafe
      Queenside
      game
      Black
    | otherwise = True
  validCastles _ = True


fastIsChecking
  :: [Move] -> BitboardRepresentation -> Coordinate -> PieceType -> Bool
fastIsChecking moves bitboard coord pt =
  not
    . null
    . filter (liftA2 (&&) moveIsCapture (moveMatchesPieceType pt))
    . filter moveIsFromTargetCoordinate
    $ moves

 where
  moveIsCapture :: Move -> Bool
  moveIsCapture (Capture _ _) = True
  moveIsCapture _ = False

  moveMatchesPieceType :: PieceType -> Move -> Bool
  moveMatchesPieceType pt =
    (== (Just pt)) . (fmap pieceType) . bitboardPieceAt bitboard . moveTo

  moveIsFromTargetCoordinate :: Move -> Bool
  moveIsFromTargetCoordinate = (== coord) . moveFrom

  moveTo (Move _ t) = t
  moveTo (Capture _ t) = t
  moveTo (Castle _ t) = t
  moveTo (EnPassant _ t) = t
  moveTo (Promote _ t _) = t

  moveFrom (Move f _) = f
  moveFrom (Capture f _) = f
  moveFrom (Castle f _) = f
  moveFrom (EnPassant f _) = f
  moveFrom (Promote f _ _) = f

isCastleSafe :: CastleSide -> Game BitboardRepresentation -> Player -> Bool
isCastleSafe Kingside game White =
  all (not . isAttacked game) [Coordinate 'f' 1, Coordinate 'g' 1]
isCastleSafe Kingside game Black =
  all (not . isAttacked game) [Coordinate 'f' 8, Coordinate 'g' 8]
isCastleSafe Queenside game White = all
  (not . isAttacked game)
  [Coordinate 'b' 1, Coordinate 'c' 1, Coordinate 'd' 1]
isCastleSafe Queenside game Black = all
  (not . isAttacked game)
  [Coordinate 'b' 8, Coordinate 'c' 8, Coordinate 'd' 8]
