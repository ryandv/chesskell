{-# LANGUAGE TupleSections #-}
module Chess.AI where

import Chess.Base
import Chess.Game
import Chess.MoveGen

import Control.Applicative

import Data.List
import Data.Maybe
import Data.Ord

data GameTree a = GameTree
  { gameTreeValue    :: a
  , gameTreeChildren :: [GameTree a]
  , gameTreeLastMove :: Maybe Move
  } deriving(Show)

instance Functor GameTree where
  fmap f (GameTree v children move) = GameTree (f v) ((fmap . fmap) f children) move

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

-- Inspired by John Hughes' "Why Functional Programming Matters"

gamesFrom      :: RegularGame -> [(Maybe Move, RegularGame)]
gamesFrom game = mapMaybe (\move -> (Just move,) <$> makeMoveFrom game move) (pseudoLegalMoves game)

gameTreeFrom          :: (RegularGame -> [RegularGame]) -> Maybe Move -> RegularGame -> GameTree RegularGame
gameTreeFrom f move game = GameTree game (map (uncurry (gameTreeFrom f)) (gamesFrom game)) move

minimize :: GameTree RegularGame -> GameTree RegularGame
minimize (GameTree v [] move) = GameTree v [] move
minimize (GameTree v children move) = minimumBy (comparing (evalGame . gameTreeValue)) (map maximize children)

maximize :: GameTree RegularGame -> GameTree RegularGame
maximize (GameTree v [] move) = GameTree v [] move
maximize (GameTree v children move) = maximumBy (comparing (evalGame . gameTreeValue)) (map minimize children)

prune 0 (GameTree v _ move) = GameTree v [] move
prune n (GameTree v children move) = GameTree v (map (prune $ n - 1) children) move

decideOnMove :: Player -> RegularGame -> Move
decideOnMove player game | player == White = fromJust $ gameTreeLastMove $ maximize $ prune 3 $ gameTreeFrom (mapMaybe (makeMoveFrom game) . pseudoLegalMoves) Nothing game
                         | otherwise = fromJust $ gameTreeLastMove $ minimize $ prune 3 $ gameTreeFrom (mapMaybe (makeMoveFrom game) . pseudoLegalMoves) Nothing game
