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

makeMoveFrom :: RegularGame -> Move -> Maybe RegularGame
makeMoveFrom game move = case moveAccepted game move of
  True  -> Just $ doMakeMove game move
  False -> Nothing

moveAccepted :: RegularGame -> Move -> Bool
moveAccepted game move =
  evalState (makeMove move) $ ChessGame game (regularGameToBitboardGame game)

doMakeMove :: RegularGame -> Move -> RegularGame
doMakeMove game move = gameState $ execState (makeMove move) $ ChessGame
  game
  (regularGameToBitboardGame game)

makeMove :: Move -> State ChessGame Bool
makeMove = validatedMakeMove where

  validatedMakeMove :: Move -> State ChessGame Bool
  validatedMakeMove move = do
    game <- get
    let toMove   = activeColor . gameState $ game
    let position = placement . gameState $ game
    if (pieceOwner <$> pieceAt position (moveFrom move)) == (Just toMove)
      then makeMove' move
      else return False

  makeMove' move@(Castle _ _) = makeCastle move
  makeMove' move@(Promote _ _ _) = makePromotion move
  makeMove' move@(EnPassant _ _) = makeEnPassant move
  makeMove' move = makeStandardMove move

makeStandardMove :: Move -> State ChessGame Bool
makeStandardMove move = do
  game <- get
  let gamestate         = gameState game
  let bitboardGameState = bitboards game
  let position          = placement bitboardGameState

  if moveIsLegal move bitboardGameState
    then do
      let originalPiece = bitboardPieceAt position (moveFrom move)

      put $ game
        { gameState = gamestate { activeColor = opponent (activeColor gamestate)
                                }
        , bitboards = bitboardGameState
                        { activeColor = opponent (activeColor gamestate)
                        }
        }
      doMovePiece originalPiece move

      return True
    else return False

makeCastle :: Move -> State ChessGame Bool
makeCastle move
  | moveFrom move == Coordinate 'e' 1 && moveTo move == Coordinate 'g' 1
  = makeWhiteKingsideCastle
  | moveFrom move == Coordinate 'e' 1 && moveTo move == Coordinate 'c' 1
  = makeWhiteQueensideCastle
  | moveFrom move == Coordinate 'e' 8 && moveTo move == Coordinate 'g' 8
  = makeBlackKingsideCastle
  | moveFrom move == Coordinate 'e' 8 && moveTo move == Coordinate 'c' 8
  = makeBlackQueensideCastle where

  makeWhiteKingsideCastle :: State ChessGame Bool
  makeWhiteKingsideCastle = doCastle
    Kingside
    disableWhiteCastles
    (Castle (Coordinate 'h' 1) (Coordinate 'f' 1))

  makeWhiteQueensideCastle :: State ChessGame Bool
  makeWhiteQueensideCastle = doCastle
    Queenside
    disableWhiteCastles
    (Castle (Coordinate 'a' 1) (Coordinate 'd' 1))

  makeBlackKingsideCastle :: State ChessGame Bool
  makeBlackKingsideCastle = doCastle
    Kingside
    disableBlackCastles
    (Castle (Coordinate 'h' 8) (Coordinate 'f' 8))

  makeBlackQueensideCastle :: State ChessGame Bool
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
    -> State ChessGame Bool
  doCastle castleSide fupdaterights rookMove = do
    game <- get
    let gamestate         = gameState game
    let bitboardGameState = bitboards game
    let position          = placement bitboardGameState
    let from = (moveFrom move)

    let originalPiece     = bitboardPieceAt position from
    let rook              = bitboardPieceAt position (moveFrom rookMove)

    if (not $ isChecked bitboardGameState
         { placement = bitboardMovePiece (bitboardMovePiece position move)
                                         rookMove
         }
       )
         && isCastleSafe castleSide bitboardGameState (activeColor gamestate)
      then do
        put $ game
          { gameState = gamestate
                          { activeColor    = opponent (activeColor gamestate)
                          , castlingRights = fupdaterights
                                               (castlingRights gamestate)
                          }
          , bitboards = bitboardGameState
                          { activeColor    = opponent (activeColor gamestate)
                          , castlingRights = fupdaterights
                                               (castlingRights gamestate)
                          }
          }

        doMovePiece originalPiece move
        doMovePiece rook          rookMove

        return True
      else return False

makePromotion :: Move -> State ChessGame Bool
makePromotion move@(Promote _ _ p) = do
  game <- get
  let position          = gameState game
  let bitboardGameState = bitboards game

  if moveIsLegal move bitboardGameState
    then do
      put $ game
        { gameState = position { activeColor = opponent (activeColor position) }
        , bitboards = bitboardGameState
                        { activeColor = opponent (activeColor position)
                        }
        }

      doMovePiece (Just p) move
      return True
    else return False

makeEnPassant :: Move -> State ChessGame Bool
makeEnPassant m = do
  game <- get
  let position          = gameState game
  let bitboardGameState = bitboards game
  let from = moveFrom m
  let (Coordinate f r)  = moveTo m

  if moveIsLegal m bitboardGameState
    then do
      let posn          = placement bitboardGameState
      let originalPiece = bitboardPieceAt posn from
      let rankOffset =
            if fmap pieceOwner originalPiece == Just White then (-1) else 1
      put $ game
        { gameState = position { activeColor = opponent (activeColor position)
                               , enPassantSquare = Nothing
                               }
        , bitboards = bitboardGameState
                        { activeColor     = opponent (activeColor position)
                        , enPassantSquare = Nothing
                        }
        }

      doMovePiece originalPiece m
      updateSquare (Coordinate f (r + rankOffset)) Nothing

      return True
    else return False

doMovePiece :: Maybe Piece -> Move -> State ChessGame ()
doMovePiece p m = do
  game <- get
  let position = placement $ gameState game
  put $ game
    { gameState = (gameState game) { placement = movePiece position p m }
    , bitboards = regularGameToBitboardGame
                    $ (gameState game) { placement = movePiece position p m }
    }

updateSquare :: Coordinate -> Maybe Piece -> State ChessGame ()
updateSquare c p = do
  game <- get
  let position = placement $ gameState game
  put $ game
    { gameState = (gameState game) { placement = addPiece position p c }
    , bitboards = regularGameToBitboardGame
                    $ (gameState game) { placement = addPiece position p c }
    }
