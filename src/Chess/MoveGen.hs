module Chess.MoveGen
  ( alongRay
  , isBlocked
  , pseudoLegalMoves
  , potentialBishopMoves
  , potentialPawnMoves
  , potentialRookMoves
  ) where

import Chess.Base

import Data.Maybe

import Control.Applicative

pseudoLegalMoves               :: RegularGame -> [(Coordinate, Coordinate)]
pseudoLegalMoves RegularGame { placement = b
                             , enPassantSquare = e
                             } = (concatMap . concatMap) (pseudoLegalMovesFrom e b) b where

pseudoLegalMovesFrom :: Maybe Coordinate -> RegularBoardRepresentation -> Square -> [(Coordinate, Coordinate)]
pseudoLegalMovesFrom _ _ (Square Nothing _)            = []
pseudoLegalMovesFrom c b (Square (Just (Piece p _)) l) | p == Pawn   = filter (unoccupied b . snd) $ potentialPawnMoves b c l
                                                       | p == Knight = filter (unoccupied b . snd) $ potentialKnightMoves l
                                                       | p == Bishop = filter (unoccupied b . snd) $ potentialBishopMoves b l
                                                       | p == Rook   = filter (unoccupied b . snd) $ potentialRookMoves b l
                                                       | p == Queen  = filter (unoccupied b . snd) $ potentialQueenMoves b l
                                                       | p == King   = filter (unoccupied b . snd) $ potentialKingMoves l

potentialPawnMoves                                       :: RegularBoardRepresentation -> Maybe Coordinate -> Coordinate -> [(Coordinate, Coordinate)]
potentialPawnMoves b Nothing c                           = standardPawnMoves b c
potentialPawnMoves b (Just enPassant) c@(Coordinate r f) = standardPawnMoves b c ++ enPassantMoves enPassant where
  rankOffset :: Int
  rankOffset = case (fmap pieceOwner $ pieceOn $ squareAt b c) of
                 Just White -> 1
                 Just Black -> -1

  enPassantMoves                      :: Coordinate -> [(Coordinate, Coordinate)]
  enPassantMoves (Coordinate r' f')   | (toEnum $ fromEnum r' + fromEnum rankOffset) == r && (f == (f' - 1) || f == (f' + 1)) = [((Coordinate r f), (Coordinate r' f'))]
                                      | otherwise                                   = []

standardPawnMoves                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
standardPawnMoves b c@(Coordinate _ r) | r == 2 && ((Just White) == pawnOwner)  = whiteDoubleJump b c ++ whiteAdvance b c ++ whiteCaptures b c
                                       | r == 7 && ((Just Black) == pawnOwner)  = blackDoubleJump b c ++ blackAdvance b c ++ blackCaptures b c
                                       | ((Just White) == pawnOwner) = whiteAdvance b c ++ whiteCaptures b c
                                       | ((Just Black) == pawnOwner) = blackAdvance b c ++ blackCaptures b c where
  pawnOwner :: Maybe Player
  pawnOwner = (fmap pieceOwner . pieceOn $ squareAt b c)

whiteAdvance                       :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
whiteAdvance b c                   = advance b c 1

blackAdvance                       :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
blackAdvance b c                   = advance b c (-1)

whiteDoubleJump                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
whiteDoubleJump b c                  = advance b c 2

blackDoubleJump                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
blackDoubleJump b c                  = advance b c (-2)

advance                             :: RegularBoardRepresentation -> Coordinate -> Rank -> [(Coordinate, Coordinate)]
advance b c@(Coordinate f r) offset | (unoccupied b $ Coordinate f (r+offset)) = [((Coordinate f r), (Coordinate f (r+offset)))]
                                    | otherwise                                = []

whiteCaptures                           :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
whiteCaptures b c@(Coordinate f r)      | f == 'a' = whiteNECapture b c
                                        | f == 'h' = whiteNWCapture b c
                                        | otherwise = whiteNWCapture b c ++ whiteNECapture b c

blackCaptures                           :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
blackCaptures b c@(Coordinate f r)      | f == 'a' = blackNECapture b c
                                        | f == 'h' = blackNWCapture b c
                                        | otherwise = blackNWCapture b c ++ blackNECapture b c

capture                                     :: RegularBoardRepresentation -> Coordinate -> File -> Rank -> Player -> [(Coordinate, Coordinate)]
capture b c@(Coordinate f r) tf dr opponent | (isJust $ target) && fmap pieceOwner target == Just opponent = [((Coordinate f r), targetCoord)]
                                            | otherwise = [] where
  target = pieceOn $ squareAt b (Coordinate tf (r+dr))
  targetCoord = Coordinate tf (r+dr)

whiteNWCapture                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
whiteNWCapture b c@(Coordinate f r) = capture b c (pred f) 1 Black

blackNWCapture                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
blackNWCapture b c@(Coordinate f r) = capture b c (pred f) (-1) White

whiteNECapture                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
whiteNECapture b c@(Coordinate f r) = capture b c (succ f) 1 Black

blackNECapture                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
blackNECapture b c@(Coordinate f r) = capture b c (succ f) (-1) White

potentialRayMoves           :: Coordinate -> [(Int, Int)] -> [(Coordinate, Coordinate)]
potentialRayMoves c offsets = fmap (\x -> (c,x)) $ filter isOnBoard $ fmap (c `offsetBy`) $ scaleBy <$> [1..7] <*> offsets

potentialBishopMoves     :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
potentialBishopMoves b c = filter (not . (isBlocked b)) $ potentialRayMoves c diagonals where
  diagonals = [(-1,1),(1,1),(1,-1),(-1,-1)]

potentialRookMoves     :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
potentialRookMoves b c = filter (not . (isBlocked b)) $ potentialRayMoves c straights where
  straights = [(1,0),(-1,0),(0,1),(0,-1)]

potentialQueenMoves     :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
potentialQueenMoves b c = potentialRookMoves b c ++ potentialBishopMoves b c

potentialKingMoves   :: Coordinate -> [(Coordinate, Coordinate)]
potentialKingMoves   = undefined

isBlocked              :: RegularBoardRepresentation -> (Coordinate, Coordinate) -> Bool
isBlocked b (from, to) = not $ to `elem` (validMoves $ alongRay (from, to)) where

  validMoves    :: [Coordinate] -> [Coordinate]
  validMoves cs = validMoves' cs False

  validMoves'                :: [Coordinate] -> Bool -> [Coordinate]
  validMoves' (c:cs) blocked | blocked == True = []
                             | otherwise       = case (fmap pieceOwner $ pieceOn $ squareAt b c) of
                                                   Nothing                -> c:validMoves' cs False
                                                   (Just owner) -> if ((Just owner) == (fmap pieceOwner $ pieceOn $ squareAt b from))
                                                                               then []
                                                                               else c:validMoves' cs True

alongRay            :: (Coordinate, Coordinate) -> [Coordinate]
alongRay (from, to) = filter (\x -> coordinateEuclideanDistance from x <= coordinateEuclideanDistance from to)
                    $ filter isOnBoard
                    $ fmap (from `offsetBy`)
                    $ scaleBy <$> [1..7] <*> [rayFromMove (from, to)]

