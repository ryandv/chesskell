{-# LANGUAGE TupleSections #-}
module Chess.AI where

import Chess.Base
import Chess.Game
import Chess.MoveGen

import Control.Applicative

import Data.List
import Data.Maybe
import Data.Ord

import Debug.Trace

evalGame      :: RegularGame -> Int
evalGame game | isCheckmate game (activeColor game) = checkmateValue
              | isStalemate game (activeColor game) = 0
              | otherwise = evalPosition $ placement game
              where

  checkmateValue | (activeColor game) == White = minBound
                 | otherwise                   = maxBound

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

miniMax :: Int -> Player -> Maybe Move -> RegularGame -> (Move, Int)
miniMax depth player lastMove game | depth == 0 = (fromJust lastMove, evalGame game)
                                   | player == White = case scoredMoves of
                                       [] -> (fromJust lastMove, evalGame game)
                                       xs -> maximumBy (comparing snd) $ xs
                                   | otherwise = case scoredMoves of
                                       [] -> (fromJust lastMove, evalGame game)
                                       xs -> minimumBy (comparing snd) $ xs
                                   where
  scoredMoves = mapMaybe (\move -> (move, ) <$> snd <$> miniMax (depth - 1) (opponent player) (Just move) <$> makeMoveFrom game move) $ pseudoLegalMoves game
--
--alphaBeta :: Int -> Player -> Maybe Move -> Int -> Int -> RegularGame -> (Move, Int)
--alphaBeta depth player alpha beta lastMove game | depth == 0 = (fromJust lastMove, evalGame game)
--                                                | player == White = case scoredMoves of
--                                                    [] -> (fromJust lastMove, evalGame game)
--                                                    xs -> maximumBy (comparing snd) $ xs
--                                                | otherwise = case scoredMoves of
--                                                    [] -> (fromJust lastMove, evalGame game)
--                                                    xs -> minimumBy (comparing snd) $ xs
--                                                where
--  scoredMoves = mapMaybe (\move -> (move, ) <$> snd <$> miniMax (depth - 1) (opponent player) (Just move) <$> makeMoveFrom game move) $ pseudoLegalMoves game

decideOnMove :: Player -> RegularGame -> Move
decideOnMove player game | player == White = fst $ miniMax 2 player Nothing game
                         | otherwise = fst $ miniMax 2 player Nothing game
