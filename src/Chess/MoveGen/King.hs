module Chess.MoveGen.King where

import Chess.Base

import Chess.MoveGen.Common

potentialKingMoves                                           :: CastleRights -> RegularBoardRepresentation -> Coordinate -> [Move]
potentialKingMoves castlerights placement c@(Coordinate f r) | f == 'e' && r == 1 && (Just White) == kingOwner = potentialOffsetMoves placement c possibleMoves ++ whiteCastles castlerights
                                                             | f == 'e' && r == 8 && (Just Black) == kingOwner = potentialOffsetMoves placement c possibleMoves ++ blackCastles castlerights
                                                             | otherwise = potentialOffsetMoves placement c possibleMoves where

  possibleMoves = [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

  kingOwner = fmap pieceOwner $ pieceAt placement c

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
  ooRookIsPresent homeRank ply = (Just (Piece Rook ply)) == (pieceAt placement $ (Coordinate 'h' homeRank))

  ooSquaresAreFree          :: Rank -> Bool
  ooSquaresAreFree homeRank = all (unoccupied placement) [(Coordinate 'f' homeRank), (Coordinate 'g' homeRank)]

  oooCastle              :: Rank -> Player -> [Move]
  oooCastle homeRank ply | oooRookIsPresent homeRank ply && oooSquaresAreFree homeRank = [Move { moveFrom = Coordinate 'e' homeRank
                                                                                               , moveTo = Coordinate 'c' homeRank
                                                                                               , moveType = Castle
                                                                                               , movePromoteTo = Nothing }]
                         | otherwise = []

  oooRookIsPresent              :: Rank -> Player -> Bool
  oooRookIsPresent homeRank ply = (Just (Piece Rook ply)) == (pieceAt placement $ (Coordinate 'a' homeRank))

  oooSquaresAreFree          :: Rank -> Bool
  oooSquaresAreFree homeRank = all (unoccupied placement) [(Coordinate 'b' homeRank), (Coordinate 'c' homeRank), (Coordinate 'd' homeRank)]
