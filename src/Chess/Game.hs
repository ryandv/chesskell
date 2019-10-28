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
  , bitboards :: BitboardRepresentation
  }

makeMoveFrom :: RegularGame -> Move -> Maybe RegularGame
makeMoveFrom game move = case moveAccepted game move of
                           True -> Just $ doMakeMove game move
                           False -> Nothing

moveAccepted :: RegularGame -> Move -> Bool
moveAccepted game move = evalState (makeMove move) $ ChessGame game (regularToBitboard $ placement game)

doMakeMove :: RegularGame -> Move -> RegularGame
doMakeMove game move = gameState $ execState (makeMove move) $ ChessGame game (regularToBitboard $ placement game )

makeMove                                   :: Move -> State ChessGame Bool
makeMove move@Move { moveType = movetype } | movetype == Castle    = makeCastle move
                                           | movetype == Promotion = makePromotion move
                                           | movetype == EnPassant = makeEnPassant move
                                           | otherwise             = makeStandardMove move

makeStandardMove                              :: Move -> State ChessGame Bool
makeStandardMove move@Move { moveFrom = from } = do
  game <- get
  let gamestate = gameState game
  let position = placement gamestate

  if moveIsLegal move (regularGameToBitboardGame gamestate)
    then do let originalPiece = pieceAt position from

            put $ game { gameState = gamestate { activeColor = opponent (activeColor gamestate) } }
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
  makeWhiteKingsideCastle = doCastle [Coordinate 'f' 1, Coordinate 'g' 1] disableWhiteCastles
    Move { moveFrom = Coordinate 'h' 1
         , moveTo   = Coordinate 'f' 1
         , moveType = Castle
         , movePromoteTo = Nothing
         }

  makeWhiteQueensideCastle :: State ChessGame Bool
  makeWhiteQueensideCastle = doCastle [Coordinate 'd' 1, Coordinate 'c' 1] disableWhiteCastles
    Move { moveFrom = Coordinate 'a' 1
         , moveTo   = Coordinate 'd' 1
         , moveType = Castle
         , movePromoteTo = Nothing
         }

  makeBlackKingsideCastle :: State ChessGame Bool
  makeBlackKingsideCastle = doCastle [Coordinate 'f' 8, Coordinate 'g' 8] disableBlackCastles
    Move { moveFrom = Coordinate 'h' 8
         , moveTo   = Coordinate 'f' 8
         , moveType = Castle
         , movePromoteTo = Nothing
         }

  makeBlackQueensideCastle :: State ChessGame Bool
  makeBlackQueensideCastle = doCastle [Coordinate 'd' 8, Coordinate 'c' 8] disableBlackCastles
    Move { moveFrom = Coordinate 'a' 8
         , moveTo   = Coordinate 'd' 8
         , moveType = Castle
         , movePromoteTo = Nothing
         }

  disableWhiteCastles :: CastleRights -> CastleRights
  disableWhiteCastles (CastleRights _ boo _ booo) = CastleRights False boo False booo

  disableBlackCastles :: CastleRights -> CastleRights
  disableBlackCastles (CastleRights woo _ wooo _) = CastleRights woo False wooo False

  doCastle :: [Coordinate] -> (CastleRights -> CastleRights) -> Move -> State ChessGame Bool
  doCastle castlingSquares fupdaterights rookMove@Move { moveFrom = rookfrom } = do
    game <- get
    let gamestate = gameState game
    let bitboardGameState = regularGameToBitboardGame gamestate
    let position = placement $ gameState game

    let originalPiece = pieceAt position from
    let rook = pieceAt position rookfrom

    if (not $ isChecked (regularGameToBitboardGame $ gamestate { placement = positionAfterMove (positionAfterMove position move) rookMove })) && (all (not . isAttacked bitboardGameState) castlingSquares)
      then do
        put $ game { gameState = gamestate { activeColor = opponent (activeColor gamestate)
                                           , castlingRights = fupdaterights (castlingRights gamestate)
                                           } }

        doMovePiece originalPiece move
        doMovePiece rook rookMove

        return True
    else return False

makePromotion :: Move -> State ChessGame Bool
makePromotion move@Move { movePromoteTo = p } = do
  game <- get
  let position = gameState game

  if moveIsLegal move (regularGameToBitboardGame position)
    then do
      put $ game { gameState = position { activeColor = opponent (activeColor position) } }

      doMovePiece p move
      return True
    else return False

makeEnPassant   :: Move -> State ChessGame Bool
makeEnPassant m@Move { moveTo = Coordinate f r
                     , moveFrom = from } = do
  game <- get
  let position = gameState game

  if moveIsLegal m (regularGameToBitboardGame position)
    then do let posn = placement position
            let originalPiece = pieceAt posn from
            let rankOffset = if fmap pieceOwner originalPiece == Just White then (-1) else 1
            put $ game { gameState = position { activeColor = opponent (activeColor position)
                                              , enPassantSquare = Nothing } }

            doMovePiece originalPiece m
            updateSquare (Coordinate f (r+rankOffset)) Nothing

            return True
    else return False

doMovePiece     :: Maybe Piece -> Move -> State ChessGame ()
doMovePiece p m = do
  game <- get
  let position = placement $ gameState game
  put $ game { gameState = (gameState game) { placement = movePiece position p m } }

updateSquare     :: Coordinate -> Maybe Piece -> State ChessGame ()
updateSquare c p = do
  game <- get
  let position = placement $ gameState game
  put $ game { gameState = (gameState game) { placement = addPiece position p c } }
