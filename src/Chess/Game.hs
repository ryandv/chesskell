module Chess.Game where

import Control.Applicative
import Control.Monad.State.Lazy

import Chess.Base
import Chess.MoveGen

import Data.Maybe

makeMoveFrom :: RegularGame -> Move -> Maybe RegularGame
makeMoveFrom game move = case (evalState (makeMove move) game) of
                           True -> Just $ execState (makeMove move) game
                           False -> Nothing

makeMove                                   :: Move -> State RegularGame Bool
makeMove move@Move { moveType = movetype } | movetype == Castle    = makeCastle move
                                           | movetype == Promotion = makePromotion move
                                           | movetype == EnPassant = makeEnPassant move
                                           | otherwise             = makeStandardMove move

moveIsLegal :: Move -> RegularGame -> Bool
moveIsLegal move@Move{ moveFrom = from} game = moveIsPseudoLegal && moveIsByRightPlayer && notCheckedAfterMove where

  moveIsPseudoLegal = (move `elem` pseudoLegalMoves game)
  moveIsByRightPlayer = (pieceOwner <$> (pieceOn $ (squareAt position from))) == (Just (activeColor game))
  notCheckedAfterMove = (not $ isChecked game { placement = positionAfterMove position move})
  position = placement game

makeStandardMove                              :: Move -> State RegularGame Bool
makeStandardMove move@Move { moveFrom = from } = do
  game <- get
  let position = placement game

  if moveIsLegal move game
    then do let originalPiece = pieceOn $ squareAt position from

            put $ game { activeColor = opponent (activeColor game) }
            doMovePiece originalPiece move

            return True
    else return False

makeCastle                              :: Move -> State RegularGame Bool
makeCastle move@Move { moveFrom = from
                     , moveTo   = to }  | from == Coordinate 'e' 1 && to == Coordinate 'g' 1 = makeWhiteKingsideCastle
                                        | from == Coordinate 'e' 1 && to == Coordinate 'c' 1 = makeWhiteQueensideCastle
                                        | from == Coordinate 'e' 8 && to == Coordinate 'g' 8 = makeBlackKingsideCastle
                                        | from == Coordinate 'e' 8 && to == Coordinate 'c' 8 = makeBlackQueensideCastle where

  makeWhiteKingsideCastle :: State RegularGame Bool
  makeWhiteKingsideCastle = doCastle [Coordinate 'f' 1, Coordinate 'g' 1] disableWhiteCastles
    Move { moveFrom = Coordinate 'h' 1
         , moveTo   = Coordinate 'f' 1
         , moveType = Castle
         , movePromoteTo = Nothing
         }

  makeWhiteQueensideCastle :: State RegularGame Bool
  makeWhiteQueensideCastle = doCastle [Coordinate 'd' 1, Coordinate 'c' 1] disableWhiteCastles
    Move { moveFrom = Coordinate 'a' 1
         , moveTo   = Coordinate 'd' 1
         , moveType = Castle
         , movePromoteTo = Nothing
         }

  makeBlackKingsideCastle :: State RegularGame Bool
  makeBlackKingsideCastle = doCastle [Coordinate 'f' 8, Coordinate 'g' 8] disableBlackCastles
    Move { moveFrom = Coordinate 'h' 8
         , moveTo   = Coordinate 'f' 8
         , moveType = Castle
         , movePromoteTo = Nothing
         }

  makeBlackQueensideCastle :: State RegularGame Bool
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

  doCastle :: [Coordinate] -> (CastleRights -> CastleRights) -> Move -> State RegularGame Bool
  doCastle castlingSquares fupdaterights rookMove@Move { moveFrom = rookfrom } = do
    game <- get
    let position = placement game

    let originalPiece = pieceOn $ squareAt position from
    let rook = pieceOn $ squareAt position rookfrom

    if (not $ isChecked game { placement = positionAfterMove (positionAfterMove position move) rookMove }) && (all (not . isAttacked game) castlingSquares)
      then do
        put $ game { activeColor = opponent (activeColor game)
                   , castlingRights = fupdaterights (castlingRights game)
                   }

        doMovePiece originalPiece move
        doMovePiece rook rookMove

        return True
    else return False

makePromotion :: Move -> State RegularGame Bool
makePromotion move@Move { movePromoteTo = p } = do
  game <- get

  if moveIsLegal move game
    then do
      put $ game { activeColor = opponent (activeColor game) }

      doMovePiece p move
      return True
    else return False

makeEnPassant   :: Move -> State RegularGame Bool
makeEnPassant m@Move { moveTo = Coordinate f r
                     , moveFrom = from } = do
  game <- get

  if moveIsLegal m game
    then do let position = placement game
            let originalPiece = pieceOn $ squareAt position from
            let rankOffset = if fmap pieceOwner originalPiece == Just White then (-1) else 1
            put $ game { activeColor = opponent (activeColor game) 
                       , enPassantSquare = Nothing }

            doMovePiece originalPiece m
            updateSquare (Coordinate f (r+rankOffset)) Nothing

            return True
    else return False

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
positionAfterMove position move@Move { moveFrom = from } = movePiece position (pieceOn $ (squareAt position from)) move

-- TODO: extract non-monadic operations
updateSquare     :: Coordinate -> Maybe Piece -> State RegularGame ()
updateSquare c p = do
  game <- get
  let position = placement game
  put $ game { placement = addPiece position p c }

noLegalMovesRemaining :: RegularGame -> Player -> Bool
noLegalMovesRemaining game ply = (null $ filter (\x -> pieceIsOwnedByPly x && (not $ isChecked game { placement = positionAfterMove (placement game) x })) $ pseudoLegalMoves game) where

  pieceIsOwnedByPly :: Move -> Bool
  pieceIsOwnedByPly Move { moveFrom = from } = (pieceOwner <$> (pieceOn $ (squareAt (placement game) from))) == (Just ply)

isCheckmate          :: RegularGame -> Player -> Bool
isCheckmate game ply = (isChecked game) && noLegalMovesRemaining game ply

isStalemate          :: RegularGame -> Player -> Bool
isStalemate game ply = (not $ isChecked game) && noLegalMovesRemaining game ply

isAttacked :: RegularGame -> Coordinate -> Bool
isAttacked game sq = isQueenChecking || isRookChecking || isBishopChecking || isKnightChecking || isPawnChecking || isKingChecking where

  nextState = (placement game)

  activePly = (activeColor game)

  isChecking            :: PieceType -> (RegularGame -> Coordinate -> [Move]) -> Bool
  isChecking pt movegen = not
                        $ null
                        $ filter (\x -> ((== Capture) $ moveType x) && ((== pt) . fromJust $ pieceType <$> (pieceOn . squareAt nextState $ moveTo x)))
                        $ movegen (game { placement = addPiece nextState (Just (Piece pt activePly)) sq }) sq

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
  isPawnChecking = isChecking Pawn potentialPawnMoves

  -- TODO: do we need to consider castling? I think not.
  isKingChecking :: Bool
  isKingChecking = isChecking King potentialKingMoves

isChecked      :: RegularGame -> Bool
isChecked game = isAttacked game (kingSquare (activeColor game)) where

  kingSquare     :: Player -> Coordinate
  kingSquare ply = location $ head $ filter ((== Just (Piece King ply)) . pieceOn) $ foldr (++) [] (placement game)
