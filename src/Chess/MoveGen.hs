module Chess.MoveGen
  ( pseudoLegalMoves
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
standardPawnMoves b c@(Coordinate _ r) | r == 2    = doubleJump b c ++ advance b c ++ captures b c
                                       | r == 7    = doubleJump b c ++ advance b c ++ captures b c
                                       | otherwise = advance b c ++ captures b c

advance                            :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
advance b c@(Coordinate f r)       = maybe [] (\p -> case (pieceOwner p) of
                                                       White -> if (unoccupied b (Coordinate f (r+1))) then [((Coordinate f r), (Coordinate f (r+1)))] else []
                                                       Black -> if (unoccupied b (Coordinate f (r-1))) then [((Coordinate f r), (Coordinate f (r-1)))] else [])
                                              (pieceOn (squareAt b c))

doubleJump                         :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
doubleJump b c@(Coordinate f r)    = maybe [] (\p -> case (pieceOwner p) of
                                                       White -> if (unoccupied b (Coordinate f (r+2))) then [((Coordinate f r), (Coordinate f (r+2)))] else []
                                                       Black -> if (unoccupied b (Coordinate f (r-2))) then [((Coordinate f r), (Coordinate f (r-2)))] else [])
                                              (pieceOn (squareAt b c))

captures                           :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
--captures b c@(Coordinate f r)      | f == 'a' = nwCapture b c ++ neCapture b c where
captures b c@(Coordinate f r)      | f == 'a' = neCapture b c
                                   | f == 'h' = nwCapture b c
                                   | otherwise = nwCapture b c ++ neCapture b c

nwCapture                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
nwCapture b c@(Coordinate f r) = maybe [] (\p -> case (pieceOwner p) of
                                                   White -> if (isJust $ whiteTarget) && fmap pieceOwner whiteTarget == Just Black
                                                              then [((Coordinate f r), (Coordinate (pred f) (r+1)))]
                                                              else []
                                                   Black -> if (isJust $ blackTarget) && fmap pieceOwner blackTarget == Just White
                                                            then [((Coordinate f r), (Coordinate (pred f) (r-1)))]
                                                            else [])
                                            (pieceOn (squareAt b c)) where
  whiteTarget = pieceOn $ squareAt b (Coordinate (pred f) (r+1))
  blackTarget = pieceOn $ squareAt b (Coordinate (pred f) (r-1))


neCapture                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
neCapture b c@(Coordinate f r) = maybe [] (\p -> case (pieceOwner p) of
                                                   White -> if (isJust $ whiteTarget) && fmap pieceOwner whiteTarget == Just Black
                                                              then [((Coordinate f r), (Coordinate (succ f) (r+1)))]
                                                              else []
                                                   Black -> if (isJust $ blackTarget) && fmap pieceOwner blackTarget == Just White
                                                              then [((Coordinate f r), (Coordinate (succ f) (r-1)))]
                                                              else [])
                                            (pieceOn (squareAt b c)) where
  whiteTarget = pieceOn $ squareAt b (Coordinate (succ f) (r+1))
  blackTarget = pieceOn $ squareAt b (Coordinate (succ f) (r-1))

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
