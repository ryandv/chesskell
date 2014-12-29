module Chess.MoveGen
  ( alongRay
  , isBlocked
  , pseudoLegalMoves
  , potentialBishopMoves
  , potentialKingMoves
  , potentialKnightMoves
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
                                                       | p == Knight = filter (unoccupied b . snd) $ potentialKnightMoves b l
                                                       | p == Bishop = filter (unoccupied b . snd) $ potentialBishopMoves b l
                                                       | p == Rook   = filter (unoccupied b . snd) $ potentialRookMoves b l
                                                       | p == Queen  = filter (unoccupied b . snd) $ potentialQueenMoves b l
                                                       | p == King   = filter (unoccupied b . snd) $ potentialKingMoves b undefined l

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
whiteCaptures b c@(Coordinate f _)      | f == 'a' = whiteNECapture b c
                                        | f == 'h' = whiteNWCapture b c
                                        | otherwise = whiteNWCapture b c ++ whiteNECapture b c

blackCaptures                           :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
blackCaptures b c@(Coordinate f _)      | f == 'a' = blackNECapture b c
                                        | f == 'h' = blackNWCapture b c
                                        | otherwise = blackNWCapture b c ++ blackNECapture b c

capture                                     :: RegularBoardRepresentation -> Coordinate -> File -> Rank -> Player -> [(Coordinate, Coordinate)]
capture b c@(Coordinate f r) tf dr opponent | (isJust $ target) && fmap pieceOwner target == Just opponent = [((Coordinate f r), targetCoord)]
                                            | otherwise = [] where
  target = pieceOn $ squareAt b (Coordinate tf (r+dr))
  targetCoord = Coordinate tf (r+dr)

whiteNWCapture                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
whiteNWCapture b c@(Coordinate f _) = capture b c (pred f) 1 Black

blackNWCapture                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
blackNWCapture b c@(Coordinate f _) = capture b c (pred f) (-1) White

whiteNECapture                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
whiteNECapture b c@(Coordinate f _) = capture b c (succ f) 1 Black

blackNECapture                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
blackNECapture b c@(Coordinate f _) = capture b c (succ f) (-1) White

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

potentialOffsetMoves             :: RegularBoardRepresentation -> Coordinate -> [(Int, Int)] -> [(Coordinate, Coordinate)]
potentialOffsetMoves b c offsets = fmap (\x -> (c,x)) $ filter (flip (unoccupiedByAlly b) (fmap pieceOwner $ pieceOn $ squareAt b c)) $ filter isOnBoard $ fmap (c `offsetBy`) offsets

potentialKnightMoves     :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
potentialKnightMoves b c = potentialOffsetMoves b c possibleJumps where
  possibleJumps = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]

--standardPawnMoves b c@(Coordinate _ r) | r == 2 && ((Just White) == pawnOwner)  = whiteDoubleJump b c ++ whiteAdvance b c ++ whiteCaptures b c
--potentialKingMoves b castles c = potentialOffsetMoves b c possibleMoves ++ castles b castles c where
potentialKingMoves                              :: RegularBoardRepresentation -> CastleRights -> Coordinate -> [(Coordinate, Coordinate)]
potentialKingMoves b castles c@(Coordinate f r) | f == 'e' && r == 1 && (Just White) == kingOwner = potentialOffsetMoves b c possibleMoves ++ whiteCastles b castles c
                                                | f == 'e' && r == 8 && (Just Black) == kingOwner = potentialOffsetMoves b c possibleMoves ++ blackCastles b castles c
                                                | otherwise = potentialOffsetMoves b c possibleMoves where
  kingOwner = fmap pieceOwner . pieceOn $ squareAt b c
  possibleMoves = [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

  whiteCastles                                                :: RegularBoardRepresentation -> CastleRights -> Coordinate -> [(Coordinate, Coordinate)]
  whiteCastles b (CastleRights oo _ ooo _) c@(Coordinate f r) | oo && ooo = whiteOOCastle b ++ whiteOOOCastle b
                                                              | oo && not ooo = whiteOOCastle b
                                                              | not oo && ooo = whiteOOOCastle b
                                                              | otherwise = []

  whiteOOCastle   :: RegularBoardRepresentation -> [(Coordinate, Coordinate)]
  whiteOOCastle b | whiteOORookIsPresent b && whiteOOSquaresAreFree b = [(Coordinate 'e' 1, Coordinate 'g' 1)]
                  | otherwise = []

  whiteOORookIsPresent   :: RegularBoardRepresentation -> Bool
  whiteOORookIsPresent b = (Just (Piece Rook White)) == (pieceOn . squareAt b $ (Coordinate 'h' 1))

  whiteOOSquaresAreFree   :: RegularBoardRepresentation -> Bool
  whiteOOSquaresAreFree b = all (unoccupied b) [(Coordinate 'f' 1), (Coordinate 'g' 1)]

  whiteOOOCastle   :: RegularBoardRepresentation -> [(Coordinate, Coordinate)]
  whiteOOOCastle b | whiteOOORookIsPresent b && whiteOOOSquaresAreFree b = [(Coordinate 'e' 1, Coordinate 'c' 1)]
                   | otherwise = []

  whiteOOORookIsPresent   :: RegularBoardRepresentation -> Bool
  whiteOOORookIsPresent b = (Just (Piece Rook White)) == (pieceOn . squareAt b $ (Coordinate 'a' 1))

  whiteOOOSquaresAreFree   :: RegularBoardRepresentation -> Bool
  whiteOOOSquaresAreFree b = all (unoccupied b) [(Coordinate 'b' 1), (Coordinate 'c' 1), (Coordinate 'd' 1)]

  blackCastles                                             :: RegularBoardRepresentation -> CastleRights -> Coordinate -> [(Coordinate, Coordinate)]
  blackCastles b (CastleRights _ oo _ ooo) c@(Coordinate f r) | oo && ooo = blackOOCastle b ++ blackOOOCastle b
                                                              | oo && not ooo = blackOOCastle b
                                                              | not oo && ooo = blackOOOCastle b
                                                              | otherwise = []

  blackOOCastle   :: RegularBoardRepresentation -> [(Coordinate, Coordinate)]
  blackOOCastle b | blackOORookIsPresent b && blackOOSquaresAreFree b = [(Coordinate 'e' 8, Coordinate 'g' 8)]
                  | otherwise = []

  blackOORookIsPresent   :: RegularBoardRepresentation -> Bool
  blackOORookIsPresent b = (Just (Piece Rook Black)) == (pieceOn . squareAt b $ (Coordinate 'h' 8))

  blackOOSquaresAreFree   :: RegularBoardRepresentation -> Bool
  blackOOSquaresAreFree b = all (unoccupied b) [(Coordinate 'f' 8), (Coordinate 'g' 8)]

  blackOOOCastle   :: RegularBoardRepresentation -> [(Coordinate, Coordinate)]
  blackOOOCastle b | blackOOORookIsPresent b && blackOOOSquaresAreFree b = [(Coordinate 'e' 8, Coordinate 'c' 8)]
                   | otherwise = []

  blackOOORookIsPresent   :: RegularBoardRepresentation -> Bool
  blackOOORookIsPresent b = (Just (Piece Rook Black)) == (pieceOn . squareAt b $ (Coordinate 'a' 8))

  blackOOOSquaresAreFree   :: RegularBoardRepresentation -> Bool
  blackOOOSquaresAreFree b = all (unoccupied b) [(Coordinate 'b' 8), (Coordinate 'c' 8), (Coordinate 'd' 8)]

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
