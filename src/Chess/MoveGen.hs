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

advance                           :: RegularBoardRepresentation -> Coordinate -> Rank -> [(Coordinate, Coordinate)]
advance b (Coordinate f r) offset | (unoccupied b $ Coordinate f (r+offset)) = [((Coordinate f r), (Coordinate f (r+offset)))]
                                  | otherwise                                = []

whiteCaptures                           :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
whiteCaptures b c@(Coordinate f _)      | f == 'a' = whiteNECapture b c
                                        | f == 'h' = whiteNWCapture b c
                                        | otherwise = whiteNWCapture b c ++ whiteNECapture b c

blackCaptures                           :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
blackCaptures b c@(Coordinate f _)      | f == 'a' = blackNECapture b c
                                        | f == 'h' = blackNWCapture b c
                                        | otherwise = blackNWCapture b c ++ blackNECapture b c

capture                                   :: RegularBoardRepresentation -> Coordinate -> File -> Rank -> Player -> [(Coordinate, Coordinate)]
capture b (Coordinate f r) tf dr opponent | (isJust $ target) && fmap pieceOwner target == Just opponent = [((Coordinate f r), targetCoord)]
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

potentialKingMoves                                   :: RegularBoardRepresentation -> CastleRights -> Coordinate -> [(Coordinate, Coordinate)]
potentialKingMoves b castlerights c@(Coordinate f r) | f == 'e' && r == 1 && (Just White) == kingOwner = potentialOffsetMoves b c possibleMoves ++ whiteCastles castlerights
                                                     | f == 'e' && r == 8 && (Just Black) == kingOwner = potentialOffsetMoves b c possibleMoves ++ blackCastles castlerights
                                                     | otherwise = potentialOffsetMoves b c possibleMoves where

  possibleMoves = [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

  kingOwner = fmap pieceOwner . pieceOn $ squareAt b c

  castles                        :: Bool -> Bool -> Player -> [(Coordinate, Coordinate)]
  castles kingside queenside ply | kingside && queenside = ooCastle (getHomeRank ply) ply ++ oooCastle (getHomeRank ply) ply
                                 | kingside && not queenside = ooCastle (getHomeRank ply) ply
                                 | not kingside && queenside = oooCastle (getHomeRank ply) ply
                                 | otherwise = []

  getHomeRank     :: Player -> Rank
  getHomeRank ply | ply == White = 1
                  | otherwise    = 8

  whiteCastles                           :: CastleRights -> [(Coordinate, Coordinate)]
  whiteCastles (CastleRights oo _ ooo _) = castles oo ooo White

  blackCastles                           :: CastleRights -> [(Coordinate, Coordinate)]
  blackCastles (CastleRights _ oo _ ooo) = castles oo ooo Black

  ooCastle              :: Rank -> Player -> [(Coordinate, Coordinate)]
  ooCastle homeRank ply | ooRookIsPresent homeRank ply && ooSquaresAreFree homeRank = [(Coordinate 'e' homeRank, Coordinate 'g' homeRank)]
                        | otherwise = []

  ooRookIsPresent              :: Rank -> Player -> Bool
  ooRookIsPresent homeRank ply = (Just (Piece Rook ply)) == (pieceOn . squareAt b $ (Coordinate 'h' homeRank))

  ooSquaresAreFree          :: Rank -> Bool
  ooSquaresAreFree homeRank = all (unoccupied b) [(Coordinate 'f' homeRank), (Coordinate 'g' homeRank)]

  oooCastle              :: Rank -> Player -> [(Coordinate, Coordinate)]
  oooCastle homeRank ply | oooRookIsPresent homeRank ply && oooSquaresAreFree homeRank = [(Coordinate 'e' homeRank, Coordinate 'c' homeRank)]
                         | otherwise = []

  oooRookIsPresent              :: Rank -> Player -> Bool
  oooRookIsPresent homeRank ply = (Just (Piece Rook ply)) == (pieceOn . squareAt b $ (Coordinate 'a' homeRank))

  oooSquaresAreFree          :: Rank -> Bool
  oooSquaresAreFree homeRank = all (unoccupied b) [(Coordinate 'b' homeRank), (Coordinate 'c' homeRank), (Coordinate 'd' homeRank)]

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
