{-# LANGUAGE TupleSections #-}
module Chess.AI where

import Chess.Base
import Chess.Game
import Chess.MoveGen

import Control.Applicative

import Data.Maybe

data GameTree = GameTree
  { gameTreeValue    :: RegularGame
  , gameTreeChildren :: [GameTree]
  , gameTreeLastMove :: Move
  } | GameTreeRoot
  { gameTreeValue    :: RegularGame
  , gameTreeChildren :: [GameTree]
  }

evalGame      :: RegularGame -> Int
evalGame game | isCheckmate game (activeColor game) = checkmateValue
              | isStalemate game (activeColor game) = 0
              | otherwise = evalPosition $ placement game
              where

  checkmateValue | activeColor game == White = minBound
                 | otherwise                 = maxBound

evalPosition          :: RegularBoardRepresentation -> Int
evalPosition position = foldr ((+) . pieceValue) 0 $ concat position where

  pieceValue :: Square -> Int
  pieceValue (Square (Just (Piece Pawn owner)) _)   | owner == White = 100
                                                    | otherwise      = -100
  pieceValue (Square (Just (Piece Knight owner)) _) | owner == White = 300
                                                    | otherwise      = -300
  pieceValue (Square (Just (Piece Bishop owner)) _) | owner == White = 300
                                                    | otherwise      = -300
  pieceValue (Square (Just (Piece Rook owner)) _)   | owner == White = 500
                                                    | otherwise      = -500
  pieceValue (Square (Just (Piece Queen owner)) _)  | owner == White = 900
                                                    | otherwise      = -900
  pieceValue _ = 0

gamesFrom      :: RegularGame -> [(Move, RegularGame)]
gamesFrom game = mapMaybe (\move -> (move,) <$> makeMoveFrom game move) (pseudoLegalMoves game)

gameTreeFrom          :: (RegularGame -> [RegularGame]) -> Move -> RegularGame -> GameTree
gameTreeFrom f _ game = GameTreeRoot game (map (uncurry (gameTreeFrom f)) (gamesFrom game))


--decideOnMove :: Player -> RegularGame -> Move
--decideOnMove player game | player == White = fst $ miniMax 2 player Nothing game
--                         | otherwise = fst $ miniMax 2 player Nothing game
