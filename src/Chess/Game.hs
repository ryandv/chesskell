module Chess.Game where

import Control.Applicative
import Control.Monad.State.Lazy

import Chess.Base
import Chess.MoveGen

import Data.Maybe

addPiece                        :: RegularBoardRepresentation -> Maybe Piece -> Coordinate -> RegularBoardRepresentation
addPiece b p c@(Coordinate f r) = newPlacement where
  newPlacement = fst splitBoard ++ [fst splitRank ++ [Square p c] ++ (tail . snd $ splitRank)] ++ (tail . snd $ splitBoard)
  splitBoard = splitAt (r - 1) b
  splitRank = splitAt (fromEnum f - fromEnum 'a') targetRank
  targetRank = head . snd $ splitBoard

doMovePiece     :: Maybe Piece -> Move -> State RegularGame ()
doMovePiece p m = do
  game <- get
  let position = placement game
  put $ game { placement = movePiece position p m }

movePiece :: RegularBoardRepresentation -> Maybe Piece -> Move -> RegularBoardRepresentation
movePiece position piece Move { moveFrom = from
                              , moveTo   = to } = addPiece (addPiece position Nothing from) piece to

positionAfterMove :: RegularBoardRepresentation -> Move -> RegularBoardRepresentation
positionAfterMove position move@Move { moveFrom = from
                                     , moveTo   = to } = movePiece position (pieceOn $ (squareAt position from)) move

-- TODO: extract non-monadic operations
updateSquare     :: Coordinate -> Maybe Piece -> State RegularGame ()
updateSquare c p = do
  game <- get
  let position = placement game
  put $ game { placement = addPiece position p c }

isCheckmate          :: RegularGame -> Player -> Bool
isCheckmate game ply = null $ filter (\x -> pieceIsOwnedByPly x && (not $ isChecked game { placement = positionAfterMove (placement game) x })) $ pseudoLegalMoves game where

  pieceIsOwnedByPly :: Move -> Bool
  pieceIsOwnedByPly Move { moveFrom = from } = (pieceOwner <$> (pieceOn $ (squareAt (placement game) from))) == (Just ply)


isChecked      :: RegularGame -> Bool
isChecked game = isQueenChecking || isRookChecking || isBishopChecking || isKnightChecking || isPawnChecking || isKingChecking where

  -- TODO: Extract
  nextState = (placement game)

  activePly = (activeColor game)

  isChecking            :: PieceType -> (RegularBoardRepresentation -> Coordinate -> [Move]) -> Bool
  isChecking pt movegen = not $ null $ filter (\x -> ((== Capture) $ moveType x) && ((== pt) . fromJust $ pieceType <$> (pieceOn . squareAt nextState $ moveTo x))) $ movegen nextState (location $ kingSquare activePly)

  isQueenChecking :: Bool
  isQueenChecking = isChecking Queen potentialQueenMoves

  isRookChecking :: Bool
  isRookChecking = isChecking Rook potentialRookMoves

  isBishopChecking :: Bool
  isBishopChecking = isChecking Bishop potentialBishopMoves

  isKnightChecking :: Bool
  isKnightChecking = isChecking Knight potentialKnightMoves

  -- TODO: do we need to consider en passant? I think not.
  isPawnChecking :: Bool
  isPawnChecking = not $ null $ filter (\x -> ((== Capture) $ moveType x) && ((== Pawn) . fromJust $ pieceType <$> (pieceOn . squareAt nextState $ moveTo x))) $ potentialPawnMoves nextState Nothing (location $ kingSquare activePly)

  -- TODO: do we need to consider castling? I think not.
  isKingChecking :: Bool
  isKingChecking = not $ null $ filter (\x -> ((== Capture) $ moveType x) && ((== King) . fromJust $ pieceType <$> (pieceOn . squareAt nextState $ moveTo x))) $ potentialKingMoves nextState (CastleRights False False False False) (location $ kingSquare activePly)

  kingSquare     :: Player -> Square
  kingSquare ply = head $ filter ((== Just (Piece King ply)) . pieceOn) $ foldr (++) [] nextState

makeStandardMove                              :: Move -> State RegularGame Bool
makeStandardMove move@Move { moveFrom = from
                           , moveTo   = to
                           , moveType = movetype } = do
  game <- get
  let position = placement game

  let moveIsPseudoLegal = (move `elem` pseudoLegalMoves game)
  let moveIsByRightPlayer = (pieceOwner <$> (pieceOn $ (squareAt position from))) == (Just (activeColor game))

  if moveIsPseudoLegal && moveIsByRightPlayer && (not $ isChecked game { placement = positionAfterMove position move})
    then do let originalPiece = pieceOn $ squareAt position from

            put $ game { activeColor = opponent (activeColor game) }
            doMovePiece originalPiece move

            return True
    else return False

disableWhiteCastles :: CastleRights -> CastleRights
disableWhiteCastles (CastleRights woo boo wooo booo) = CastleRights False boo False booo

disableBlackCastles :: CastleRights -> CastleRights
disableBlackCastles (CastleRights woo boo wooo booo) = CastleRights woo False wooo False

doCastle :: (CastleRights -> CastleRights) -> Player -> Move -> Move -> State RegularGame Bool
doCastle fupdaterights ply move@Move { moveFrom = from, moveTo = to } rookMove@Move { moveFrom = rookfrom
                                                                                    , moveTo   = rookto } = do
  game <- get
  let position = placement game

  let originalPiece = pieceOn $ squareAt position from
  let rook = pieceOn $ squareAt position rookfrom

  put $ game { activeColor = opponent (activeColor game)
             , castlingRights = fupdaterights (castlingRights game)
             }

  doMovePiece originalPiece move
  doMovePiece rook rookMove

  return True

makeWhiteKingsideCastle      :: Move -> State RegularGame Bool
makeWhiteKingsideCastle move = doCastle disableWhiteCastles White move
  Move { moveFrom = Coordinate 'h' 1
       , moveTo   = Coordinate 'f' 1
       , moveType = Castle
       }

makeWhiteQueensideCastle      :: Move -> State RegularGame Bool
makeWhiteQueensideCastle move = doCastle disableWhiteCastles White move
  Move { moveFrom = Coordinate 'a' 1
       , moveTo   = Coordinate 'd' 1
       , moveType = Castle
       }

makeBlackKingsideCastle      :: Move -> State RegularGame Bool
makeBlackKingsideCastle move = doCastle disableBlackCastles Black move
  Move { moveFrom = Coordinate 'h' 8
       , moveTo   = Coordinate 'f' 8
       , moveType = Castle
       }

makeBlackQueensideCastle      :: Move -> State RegularGame Bool
makeBlackQueensideCastle move = doCastle disableBlackCastles Black move
  Move { moveFrom = Coordinate 'a' 8
       , moveTo   = Coordinate 'd' 8
       , moveType = Castle
       }

makeCastle                              :: Move -> State RegularGame Bool
makeCastle move@Move { moveFrom = from
                     , moveTo   = to }  | from == Coordinate 'e' 1 && to == Coordinate 'g' 1 = makeWhiteKingsideCastle move
                                        | from == Coordinate 'e' 1 && to == Coordinate 'c' 1 = makeWhiteQueensideCastle move
                                        | from == Coordinate 'e' 8 && to == Coordinate 'g' 8 = makeBlackKingsideCastle move
                                        | from == Coordinate 'e' 8 && to == Coordinate 'c' 8 = makeBlackQueensideCastle move

makePromotion :: Maybe Piece -> Move -> State RegularGame Bool
makePromotion p@(Just Piece { pieceType  = pt, pieceOwner = o }) move@Move { moveFrom = from, moveTo = to } = do
  game <- get
  let position = placement game

  put $ game { activeColor = opponent (activeColor game) }

  doMovePiece p move

  return True

makeEnPassant   :: Move -> State RegularGame Bool
makeEnPassant m@Move { moveTo = to@(Coordinate f r)
                     , moveFrom = from } = do
  game <- get
  if (m `elem` pseudoLegalMoves game)
    then do let position = placement game
            let originalPiece = pieceOn $ squareAt position from
            let rankOffset = if fmap pieceOwner originalPiece == Just White then (-1) else 1
            put $ game { activeColor = opponent (activeColor game) 
                       , enPassantSquare = Nothing }

            let enpassantpawn = pieceOn . squareAt position $ Coordinate f (r+rankOffset)

            doMovePiece originalPiece m
            updateSquare (Coordinate f (r+rankOffset)) Nothing

            return True
    else return False


makeMove                                             :: Maybe Piece -> Move -> State RegularGame Bool
makeMove promoteTo move@Move { moveType = movetype } | movetype == Standard  = makeStandardMove move
                                                     | movetype == Capture   = makeStandardMove move
                                                     | movetype == Castle    = makeCastle move
                                                     | movetype == Promotion = makePromotion promoteTo move
                                                     | movetype == EnPassant = makeEnPassant move
