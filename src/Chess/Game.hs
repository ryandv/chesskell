{-# LANGUAGE DoAndIfThenElse #-}
module Chess.Game
  ( ChessGame(..)
  , makeMove
  , makeMoveFrom
  , doMakeMove
  , moveAccepted
  ) where

import           Chess.Base
import           Chess.Board
import           Chess.Bitboard
import           Chess.Predicates

import           Control.Applicative
import           Control.Monad.State.Lazy

import           Data.Maybe

data ChessGame = ChessGame
  { gameState :: RegularGame
  , bitboards :: Game BitboardRepresentation
  }

makeMoveFrom :: (Game BitboardRepresentation) -> Move -> Maybe (Game BitboardRepresentation)
makeMoveFrom game move = case moveAccepted game move of
  True  -> Just $ doMakeMove game move
  False -> Nothing

moveAccepted :: (Game BitboardRepresentation) -> Move -> Bool
moveAccepted game move =
  evalState (makeMove move) game

doMakeMove :: (Game BitboardRepresentation) -> Move -> (Game BitboardRepresentation)
doMakeMove game move = execState (makeMove move) game

makeMove :: Move -> State (Game BitboardRepresentation) Bool
makeMove = validatedMakeMove where

  validatedMakeMove :: Move -> State (Game BitboardRepresentation) Bool
  validatedMakeMove move = do
    game <- get
    let toMove   = activeColor game
    if bitboardOwnerAt (placement game) (moveFrom move) == (Just toMove)
      then makeMove' move
      else return False

  makeMove' move@(Castle _ _) = makeCastle move
  makeMove' move@(Promote _ _ _) = makePromotion move
  makeMove' move@(EnPassant _ _) = makeEnPassant move
  makeMove' move = makeStandardMove move

makeStandardMove :: Move -> State (Game BitboardRepresentation) Bool
makeStandardMove move = do
  game <- get

  if moveIsLegal move game
    then do
      let originalPiece = bitboardPieceAt (placement game) (moveFrom move)

      put $ game { activeColor = opponent (activeColor game) }
      doMovePiece originalPiece move

      return True
    else return False

makeCastle :: Move -> State (Game BitboardRepresentation) Bool
makeCastle move
  | moveFrom move == Coordinate 'e' 1 && moveTo move == Coordinate 'g' 1
  = makeWhiteKingsideCastle
  | moveFrom move == Coordinate 'e' 1 && moveTo move == Coordinate 'c' 1
  = makeWhiteQueensideCastle
  | moveFrom move == Coordinate 'e' 8 && moveTo move == Coordinate 'g' 8
  = makeBlackKingsideCastle
  | moveFrom move == Coordinate 'e' 8 && moveTo move == Coordinate 'c' 8
  = makeBlackQueensideCastle where

  makeWhiteKingsideCastle :: State (Game BitboardRepresentation) Bool
  makeWhiteKingsideCastle = doCastle
    Kingside
    disableWhiteCastles
    (Castle (Coordinate 'h' 1) (Coordinate 'f' 1))

  makeWhiteQueensideCastle :: State (Game BitboardRepresentation) Bool
  makeWhiteQueensideCastle = doCastle
    Queenside
    disableWhiteCastles
    (Castle (Coordinate 'a' 1) (Coordinate 'd' 1))

  makeBlackKingsideCastle :: State (Game BitboardRepresentation) Bool
  makeBlackKingsideCastle = doCastle
    Kingside
    disableBlackCastles
    (Castle (Coordinate 'h' 8) (Coordinate 'f' 8))

  makeBlackQueensideCastle :: State (Game BitboardRepresentation) Bool
  makeBlackQueensideCastle = doCastle
    Queenside
    disableBlackCastles
    (Castle (Coordinate 'a' 8) (Coordinate 'd' 8))

  disableWhiteCastles :: CastleRights -> CastleRights
  disableWhiteCastles (CastleRights _ boo _ booo) =
    CastleRights False boo False booo

  disableBlackCastles :: CastleRights -> CastleRights
  disableBlackCastles (CastleRights woo _ wooo _) =
    CastleRights woo False wooo False

  doCastle
    :: CastleSide
    -> (CastleRights -> CastleRights)
    -> Move
    -> State (Game BitboardRepresentation) Bool
  doCastle castleSide fupdaterights rookMove = do
    game <- get
    let position          = placement game
    let from = (moveFrom move)

    let originalPiece     = bitboardPieceAt position from
    let rook              = bitboardSpecificPieceAt position (moveFrom rookMove) Rook

    if (not $ isChecked game
         { placement = bitboardMovePiece (bitboardMovePiece position move)
                                         rookMove
         }
       )
         && isCastleSafe castleSide game (activeColor game)
      then do
        put $ game { activeColor    = opponent (activeColor game)
                   , castlingRights = fupdaterights (castlingRights game)
                   }

        doMovePiece originalPiece move
        doMovePiece rook          rookMove

        return True
      else return False

makePromotion :: Move -> State (Game BitboardRepresentation) Bool
makePromotion move@(Promote _ _ p) = do
  game <- get

  if moveIsLegal move game
    then do
      put $ game { activeColor = opponent (activeColor game) }

      doMovePiece (Just p) move
      return True
    else return False

makeEnPassant :: Move -> State (Game BitboardRepresentation) Bool
makeEnPassant m = do
  game <- get
  let from = moveFrom m
  let (Coordinate f r)  = moveTo m

  if moveIsLegal m game
    then do
      let posn          = placement game
      let originalPiece = bitboardPieceAt posn from
      let rankOffset =
            if fmap pieceOwner originalPiece == Just White then (-1) else 1
      put $ game { activeColor     = opponent (activeColor game)
                 , enPassantSquare = Nothing
                 }

      doMovePiece originalPiece m
      updateSquare (Coordinate f (r + rankOffset)) Nothing

      return True
    else return False

doMovePiece :: Maybe Piece -> Move -> State (Game BitboardRepresentation) ()
doMovePiece p m = do
  game <- get
  put $ game { placement = bitboardMovePiece (placement game) m }

updateSquare :: Coordinate -> Maybe Piece -> State (Game BitboardRepresentation) ()
updateSquare c p = do
  game <- get
  put $ game { placement = addPieceTo (placement game) p c }
