module Chess.MoveGen.King where

import Chess.Base
import Chess.Board
import Chess.Bitboard

import Chess.MoveGen.Common

potentialKingMoves                                              :: CastleRights -> BitboardRepresentation -> Player -> Coordinate -> [Move]
potentialKingMoves castlerights bitboard ply c@(Coordinate f r) | f == 'e' && r == 1 && (Just ply) == kingOwner = potentialOffsetMoves possibleMoves bitboard ply c ++ whiteCastles castlerights
                                                                | f == 'e' && r == 8 && (Just ply) == kingOwner = potentialOffsetMoves possibleMoves bitboard ply c ++ blackCastles castlerights
                                                                | otherwise = potentialOffsetMoves possibleMoves bitboard ply c where

  possibleMoves = [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

  kingOwner = fmap pieceOwner $ bitboardSpecificPieceAt bitboard c King

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
  ooCastle homeRank ply | ooRookIsPresent homeRank ply && ooSquaresAreFree homeRank = [Castle (Coordinate 'e' homeRank) (Coordinate 'g' homeRank)]
                        | otherwise = []

  ooRookIsPresent              :: Rank -> Player -> Bool
  ooRookIsPresent homeRank ply = (Just (Piece Rook ply)) == (bitboardPieceAt bitboard $ (Coordinate 'h' homeRank))

  ooSquaresAreFree          :: Rank -> Bool
  ooSquaresAreFree homeRank = all (not . bitboardIsOccupied bitboard) [(Coordinate 'f' homeRank), (Coordinate 'g' homeRank)]

  oooCastle              :: Rank -> Player -> [Move]
  oooCastle homeRank ply | oooRookIsPresent homeRank ply && oooSquaresAreFree homeRank = [Castle (Coordinate 'e' homeRank) (Coordinate 'c' homeRank)]
                         | otherwise = []

  oooRookIsPresent              :: Rank -> Player -> Bool
  oooRookIsPresent homeRank ply = (Just (Piece Rook ply)) == (bitboardPieceAt bitboard $ (Coordinate 'a' homeRank))

  oooSquaresAreFree          :: Rank -> Bool
  oooSquaresAreFree homeRank = all (not . bitboardIsOccupied bitboard) [(Coordinate 'b' homeRank), (Coordinate 'c' homeRank), (Coordinate 'd' homeRank)]
