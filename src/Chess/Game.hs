{-# LANGUAGE DoAndIfThenElse #-} 
module Chess.Game
  ( ChessGame(..)
  , makeMove
  , makeMoveFrom
  , doMakeMove
  , moveAccepted
  ) where

import Chess.Base
import Chess.Board
import Chess.Bitboard
import Chess.Predicates

import Control.Applicative
import Control.Monad.State.Lazy

import Data.Maybe

data ChessGame = ChessGame
  { gameState :: RegularGame
  , bitboards :: Game BitboardRepresentation
  }

makeMoveFrom :: RegularGame -> Move -> Maybe RegularGame
makeMoveFrom game move = case moveAccepted game move of
                           True -> Just $ doMakeMove game move
                           False -> Nothing

moveAccepted :: RegularGame -> Move -> Bool
moveAccepted game move = evalState (makeMove move) $ ChessGame game (regularGameToBitboardGame game)

doMakeMove :: RegularGame -> Move -> RegularGame
doMakeMove game move = gameState $ execState (makeMove move) $ ChessGame game (regularGameToBitboardGame game )

makeMove :: Move -> State ChessGame Bool
makeMove = validatedMakeMove where

  validatedMakeMove :: Move -> State ChessGame Bool
  validatedMakeMove move@Move { moveFrom = from } = do
    game <- get
    let toMove = activeColor . gameState $ game
    let position = placement . gameState $ game
    if (pieceOwner <$> pieceAt position from) == (Just toMove)
      then makeMove' move
      else return False

  makeMove' move@Move { moveType = movetype } | movetype == Castle    = makeCastle move
                                              | movetype == Promotion = makePromotion move
                                              | movetype == EnPassant = makeEnPassant move
                                              | otherwise             = makeStandardMove move

makeStandardMove                              :: Move -> State ChessGame Bool
makeStandardMove move@Move { moveFrom = from } = do
  game <- get
  let gamestate = gameState game
  let bitboardGameState = bitboards game
  let position = placement bitboardGameState

  if moveIsLegal move bitboardGameState
    then do let originalPiece = bitboardPieceAt position from

            put $ game {
                gameState = gamestate { activeColor = opponent (activeColor gamestate) }
              , bitboards = bitboardGameState { activeColor = opponent (activeColor gamestate) }
              }
            doMovePiece originalPiece move

            return True
    else return False

makeCastle                              :: Move -> State ChessGame Bool
makeCastle move@Move { moveFrom = from
                     , moveTo   = to }  | from == Coordinate 'e' 1 && to == Coordinate 'g' 1 = makeWhiteKingsideCastle
                                        | from == Coordinate 'e' 1 && to == Coordinate 'c' 1 = makeWhiteQueensideCastle
                                        | from == Coordinate 'e' 8 && to == Coordinate 'g' 8 = makeBlackKingsideCastle
                                        | from == Coordinate 'e' 8 && to == Coordinate 'c' 8 = makeBlackQueensideCastle where

  makeWhiteKingsideCastle :: State ChessGame Bool
  makeWhiteKingsideCastle = doCastle Kingside disableWhiteCastles
    Move { moveFrom = Coordinate 'h' 1
         , moveTo   = Coordinate 'f' 1
         , moveType = Castle
         , movePromoteTo = Nothing
         }

  makeWhiteQueensideCastle :: State ChessGame Bool
  makeWhiteQueensideCastle = doCastle Queenside disableWhiteCastles
    Move { moveFrom = Coordinate 'a' 1
         , moveTo   = Coordinate 'd' 1
         , moveType = Castle
         , movePromoteTo = Nothing
         }

  makeBlackKingsideCastle :: State ChessGame Bool
  makeBlackKingsideCastle = doCastle Kingside disableBlackCastles
    Move { moveFrom = Coordinate 'h' 8
         , moveTo   = Coordinate 'f' 8
         , moveType = Castle
         , movePromoteTo = Nothing
         }

  makeBlackQueensideCastle :: State ChessGame Bool
  makeBlackQueensideCastle = doCastle Queenside disableBlackCastles
    Move { moveFrom = Coordinate 'a' 8
         , moveTo   = Coordinate 'd' 8
         , moveType = Castle
         , movePromoteTo = Nothing
         }

  disableWhiteCastles :: CastleRights -> CastleRights
  disableWhiteCastles (CastleRights _ boo _ booo) = CastleRights False boo False booo

  disableBlackCastles :: CastleRights -> CastleRights
  disableBlackCastles (CastleRights woo _ wooo _) = CastleRights woo False wooo False

  doCastle :: CastleSide -> (CastleRights -> CastleRights) -> Move -> State ChessGame Bool
  doCastle castleSide fupdaterights rookMove@Move { moveFrom = rookfrom } = do
    game <- get
    let gamestate = gameState game
    let bitboardGameState = bitboards game
    let position = placement bitboardGameState

    let originalPiece = bitboardPieceAt position from
    let rook = bitboardPieceAt position rookfrom

    if (not $ isChecked bitboardGameState { placement = bitboardMovePiece (bitboardMovePiece position move) rookMove }) && isCastleSafe castleSide bitboardGameState (activeColor gamestate)
      then do
        put $ game {
            gameState = gamestate { activeColor = opponent (activeColor gamestate)
                                  , castlingRights = fupdaterights (castlingRights gamestate)
                                  }
          , bitboards = bitboardGameState { activeColor = opponent (activeColor gamestate)
                                          , castlingRights = fupdaterights (castlingRights gamestate)
                                          }
          }

        doMovePiece originalPiece move
        doMovePiece rook rookMove

        return True
    else return False

makePromotion :: Move -> State ChessGame Bool
makePromotion move@Move { movePromoteTo = p } = do
  game <- get
  let position = gameState game
  let bitboardGameState = bitboards game

  if moveIsLegal move bitboardGameState
    then do
      put $ game {
          gameState = position { activeColor = opponent (activeColor position) }
        , bitboards = bitboardGameState { activeColor = opponent (activeColor position) }
        }

      doMovePiece p move
      return True
    else return False

makeEnPassant   :: Move -> State ChessGame Bool
makeEnPassant m@Move { moveTo = Coordinate f r
                     , moveFrom = from } = do
  game <- get
  let position = gameState game
  let bitboardGameState = bitboards game

  if moveIsLegal m bitboardGameState
    then do let posn = placement bitboardGameState
            let originalPiece = bitboardPieceAt posn from
            let rankOffset = if fmap pieceOwner originalPiece == Just White then (-1) else 1
            put $ game {
                gameState = position { activeColor = opponent (activeColor position) , enPassantSquare = Nothing }
              , bitboards = bitboardGameState { activeColor = opponent (activeColor position) , enPassantSquare = Nothing }
              }

            doMovePiece originalPiece m
            updateSquare (Coordinate f (r+rankOffset)) Nothing

            return True
    else return False

doMovePiece     :: Maybe Piece -> Move -> State ChessGame ()
doMovePiece p m = do
  game <- get
  let position = placement $ gameState game
  put $ game {
      gameState = (gameState game) { placement = movePiece position p m }
    , bitboards = regularGameToBitboardGame $ (gameState game) { placement = movePiece position p m }
    }

updateSquare     :: Coordinate -> Maybe Piece -> State ChessGame ()
updateSquare c p = do
  game <- get
  let position = placement $ gameState game
  put $ game {
       gameState = (gameState game) { placement = addPiece position p c }
     , bitboards = regularGameToBitboardGame $ (gameState game) { placement = addPiece position p c }
     }
