module Chess.MoveGen
  ( alongRay
  , isBlocked
  , pseudoLegalMoves
  , potentialBishopMoves
  , potentialKingMoves
  , potentialKnightMoves
  , potentialPawnMoves
  , potentialQueenMoves
  , potentialRookMoves
  ) where

import Chess.Base

import Data.Maybe

import Control.Applicative

pseudoLegalMoves               :: RegularGame -> [Move]
pseudoLegalMoves game@RegularGame { placement = b } = (concatMap . concatMap) (pseudoLegalMovesFrom game) b where

pseudoLegalMovesFrom :: RegularGame -> Square -> [Move]
pseudoLegalMovesFrom _ (Square Nothing _)            = []
pseudoLegalMovesFrom game (Square (Just (Piece p _)) l) | p == Pawn   = potentialPawnMoves game l
                                                        | p == Knight = potentialKnightMoves game l
                                                        | p == Bishop = potentialBishopMoves game l
                                                        | p == Rook   = potentialRookMoves game l
                                                        | p == Queen  = potentialQueenMoves game l
                                                        | p == King   = potentialKingMoves game l

potentialPawnMoves                                       :: RegularGame -> Coordinate -> [Move]
potentialPawnMoves RegularGame { placement = b, enPassantSquare = Nothing } c                           = standardPawnMoves b c
potentialPawnMoves RegularGame { placement = b, enPassantSquare = (Just enPassant) } c@(Coordinate r f) = standardPawnMoves b c ++ enPassantMoves enPassant where
  rankOffset :: Int
  rankOffset = case (fmap pieceOwner $ pieceOn $ squareAt b c) of
                 Just White -> 1
                 Just Black -> -1

  enPassantMoves                      :: Coordinate -> [Move]
  enPassantMoves (Coordinate r' f')   | (toEnum $ fromEnum r' + fromEnum rankOffset) == r
                                        && (f == (f' - 1) || f == (f' + 1))         = [Move { moveFrom = (Coordinate r f)
                                                                                            , moveTo = (Coordinate r' f')
                                                                                            , moveType = EnPassant
                                                                                            , movePromoteTo = Nothing }]
                                      | otherwise                                   = []

standardPawnMoves                      :: RegularBoardRepresentation -> Coordinate -> [Move]
standardPawnMoves b c@(Coordinate _ r) | r == 2 && ((Just White) == pawnOwner)  = whiteDoubleJump b c ++ whiteAdvance b c ++ whiteCaptures b c
                                       | r == 7 && ((Just Black) == pawnOwner)  = blackDoubleJump b c ++ blackAdvance b c ++ blackCaptures b c
                                       | ((Just White) == pawnOwner) = whiteAdvance b c ++ whiteCaptures b c
                                       | ((Just Black) == pawnOwner) = blackAdvance b c ++ blackCaptures b c where
  pawnOwner :: Maybe Player
  pawnOwner = (fmap pieceOwner . pieceOn $ squareAt b c)

whiteAdvance                       :: RegularBoardRepresentation -> Coordinate -> [Move]
whiteAdvance b c                   = advance b c 1

blackAdvance                       :: RegularBoardRepresentation -> Coordinate -> [Move]
blackAdvance b c                   = advance b c (-1)

whiteDoubleJump                      :: RegularBoardRepresentation -> Coordinate -> [Move]
whiteDoubleJump b c                  = advance b c 2

blackDoubleJump                      :: RegularBoardRepresentation -> Coordinate -> [Move]
blackDoubleJump b c                  = advance b c (-2)

advance                             :: RegularBoardRepresentation -> Coordinate -> Rank -> [Move]
advance b c@(Coordinate f r) offset | (r+offset) > 8 || (r+offset) < 1 = []
                                    | (r+offset) == 8 && ((pieceOn $ squareAt b c) == (Just $ Piece Pawn White)) = map (Move (Coordinate f r) (Coordinate f (r+offset)) Promotion)
                                      [ Just $ Piece Rook White
                                      , Just $ Piece Knight White
                                      , Just $ Piece Bishop White
                                      , Just $ Piece Queen White ]
                                    | (unoccupied b $ Coordinate f (r+offset)) = [Move { moveFrom = (Coordinate f r)
                                                                                     , moveTo = (Coordinate f (r+offset))
                                                                                     , moveType = Standard
                                                                                     , movePromoteTo = Nothing}]
                                  | otherwise                                = []

whiteCaptures                           :: RegularBoardRepresentation -> Coordinate -> [Move]
whiteCaptures b c@(Coordinate f _)      | f == 'a' = whiteNECapture b c
                                        | f == 'h' = whiteNWCapture b c
                                        | otherwise = whiteNWCapture b c ++ whiteNECapture b c

blackCaptures                           :: RegularBoardRepresentation -> Coordinate -> [Move]
blackCaptures b c@(Coordinate f _)      | f == 'a' = blackNECapture b c
                                        | f == 'h' = blackNWCapture b c
                                        | otherwise = blackNWCapture b c ++ blackNECapture b c

capture                                :: RegularBoardRepresentation -> Coordinate -> File -> Rank -> Player -> [Move]
capture b (Coordinate f r) tf dr enemy | (r+dr) > 8 || (r+dr) < 1 = []
                                       | (isJust $ target) && fmap pieceOwner target == Just enemy = [Move { moveFrom = (Coordinate f r)
                                                                                                           , moveTo = targetCoord
                                                                                                           , moveType = Capture
                                                                                                           , movePromoteTo = Nothing }]
                                       | otherwise = [] where
  target = pieceOn $ squareAt b (Coordinate tf (r+dr))
  targetCoord = Coordinate tf (r+dr)

whiteNWCapture                      :: RegularBoardRepresentation -> Coordinate -> [Move]
whiteNWCapture b c@(Coordinate f _) = capture b c (pred f) 1 Black

blackNWCapture                      :: RegularBoardRepresentation -> Coordinate -> [Move]
blackNWCapture b c@(Coordinate f _) = capture b c (pred f) (-1) White

whiteNECapture                      :: RegularBoardRepresentation -> Coordinate -> [Move]
whiteNECapture b c@(Coordinate f _) = capture b c (succ f) 1 Black

blackNECapture                      :: RegularBoardRepresentation -> Coordinate -> [Move]
blackNECapture b c@(Coordinate f _) = capture b c (succ f) (-1) White

potentialRayMoves             :: RegularBoardRepresentation -> Coordinate -> [(Int, Int)] -> [Move]
potentialRayMoves b c offsets = fmap (\x -> Move { moveFrom = c
                                                 , moveTo = x
                                                 , moveType = determineMoveType b c x
                                                 , movePromoteTo = Nothing }) $ filter isOnBoard $ fmap (c `offsetBy`) $ scaleBy <$> [1..7] <*> offsets where

determineMoveType                 :: RegularBoardRepresentation -> Coordinate -> Coordinate -> MoveType
determineMoveType b from to       | isNothing . pieceOn $ squareAt b to                          = Standard
                                  | (fmap pieceOwner . pieceOn $ squareAt b to) /= (fmap pieceOwner . pieceOn $ squareAt b from) = Capture

potentialBishopMoves     :: RegularGame -> Coordinate -> [Move]
potentialBishopMoves RegularGame { placement = b } c = filter (not . (isBlocked b)) $ potentialRayMoves b c diagonals where
  diagonals = [(-1,1),(1,1),(1,-1),(-1,-1)]

potentialRookMoves     :: RegularGame -> Coordinate -> [Move]
potentialRookMoves RegularGame { placement = b } c = filter (not . (isBlocked b)) $ potentialRayMoves b c straights where
  straights = [(1,0),(-1,0),(0,1),(0,-1)]

potentialQueenMoves     :: RegularGame -> Coordinate -> [Move]
potentialQueenMoves g c = potentialRookMoves g c ++ potentialBishopMoves g c

potentialOffsetMoves             :: RegularBoardRepresentation -> Coordinate -> [(Int, Int)] -> [Move]
potentialOffsetMoves b c offsets = fmap (\x -> Move { moveFrom = c
                                                    , moveTo = x
                                                    , moveType = determineMoveType b c x 
                                                    , movePromoteTo = Nothing }) $ filter (flip (unoccupiedByAlly b) (fmap pieceOwner $ pieceOn $ squareAt b c)) $ filter isOnBoard $ fmap (c `offsetBy`) offsets

potentialKnightMoves     :: RegularGame -> Coordinate -> [Move]
potentialKnightMoves RegularGame { placement = b } c = potentialOffsetMoves b c possibleJumps where
  possibleJumps = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]

potentialKingMoves                                   :: RegularGame -> Coordinate -> [Move]
potentialKingMoves RegularGame { placement = b
                               , castlingRights = castlerights
                               } c@(Coordinate f r) | f == 'e' && r == 1 && (Just White) == kingOwner = potentialOffsetMoves b c possibleMoves ++ whiteCastles castlerights
                                                    | f == 'e' && r == 8 && (Just Black) == kingOwner = potentialOffsetMoves b c possibleMoves ++ blackCastles castlerights
                                                    | otherwise = potentialOffsetMoves b c possibleMoves where

  possibleMoves = [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

  kingOwner = fmap pieceOwner . pieceOn $ squareAt b c

  castles                        :: Bool -> Bool -> Player -> [Move]
  castles kingside queenside ply | kingside && queenside = ooCastle (getHomeRank ply) ply ++ oooCastle (getHomeRank ply) ply
                                 | kingside && not queenside = ooCastle (getHomeRank ply) ply
                                 | not kingside && queenside = oooCastle (getHomeRank ply) ply
                                 | otherwise = []

  getHomeRank     :: Player -> Rank
  getHomeRank ply | ply == White = 1
                  | otherwise    = 8

  whiteCastles                           :: CastleRights -> [Move]
  whiteCastles (CastleRights oo _ ooo _) = castles oo ooo White

  blackCastles                           :: CastleRights -> [Move]
  blackCastles (CastleRights _ oo _ ooo) = castles oo ooo Black

  ooCastle              :: Rank -> Player -> [Move]
  ooCastle homeRank ply | ooRookIsPresent homeRank ply && ooSquaresAreFree homeRank = [Move { moveFrom = Coordinate 'e' homeRank
                                                                                            , moveTo = Coordinate 'g' homeRank
                                                                                            , moveType = Castle
                                                                                            , movePromoteTo = Nothing }]
                        | otherwise = []

  ooRookIsPresent              :: Rank -> Player -> Bool
  ooRookIsPresent homeRank ply = (Just (Piece Rook ply)) == (pieceOn . squareAt b $ (Coordinate 'h' homeRank))

  ooSquaresAreFree          :: Rank -> Bool
  ooSquaresAreFree homeRank = all (unoccupied b) [(Coordinate 'f' homeRank), (Coordinate 'g' homeRank)]

  oooCastle              :: Rank -> Player -> [Move]
  oooCastle homeRank ply | oooRookIsPresent homeRank ply && oooSquaresAreFree homeRank = [Move { moveFrom = Coordinate 'e' homeRank
                                                                                               , moveTo = Coordinate 'c' homeRank
                                                                                               , moveType = Castle
                                                                                               , movePromoteTo = Nothing }]
                         | otherwise = []

  oooRookIsPresent              :: Rank -> Player -> Bool
  oooRookIsPresent homeRank ply = (Just (Piece Rook ply)) == (pieceOn . squareAt b $ (Coordinate 'a' homeRank))

  oooSquaresAreFree          :: Rank -> Bool
  oooSquaresAreFree homeRank = all (unoccupied b) [(Coordinate 'b' homeRank), (Coordinate 'c' homeRank), (Coordinate 'd' homeRank)]

isBlocked                       :: RegularBoardRepresentation -> Move -> Bool
isBlocked b Move { moveFrom = from
                 , moveTo = to }  = not $ to `elem` (validMoves $ alongRay (from, to)) where

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

rayFromMove                                    :: (Coordinate, Coordinate) -> (Int, Int)
rayFromMove (Coordinate f r, Coordinate f' r') | fromEnum f' > fromEnum f && r' > r = (1,1)
                                               | fromEnum f' > fromEnum f && r' < r = (1,-1)
                                               | fromEnum f' > fromEnum f && r' == r = (1,0)
                                               | fromEnum f' < fromEnum f && r' > r = (-1,1)
                                               | fromEnum f' < fromEnum f && r' < r = (-1,-1)
                                               | fromEnum f' < fromEnum f && r' == r = (-1,0)
                                               | fromEnum f' == fromEnum f && r' > r = (0,1)
                                               | fromEnum f' == fromEnum f && r' < r = (0,-1)
                                               | fromEnum f' == fromEnum f && r' == r = (0,0)
