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
  , gameTreeLastMove :: [Move]
  } deriving(Show)

instance Functor GameTree where
  fmap f (GameTree v children moves) = GameTree (f v) ((fmap . fmap) f children) moves

instance Ord RegularGame where
  compare = comparing evalGame

whitePawnPieceSquareTable :: [[Int]]
whitePawnPieceSquareTable =
  [ [  0,   0,   0,   0,   0,   0,    0,   0]
  , [ 50,  50,  50,  50,  50,  50,   50,  50]
  , [ 10,  10,  20,  30,  30,  20,   10,  10]
  , [  5,   5,  10,  27,  27,  10,    5,   5]
  , [  0,   0,   0,  25,  25,   0,    0,   0]
  , [  5,  -5, -10,   0,   0,  -10,  -5,   5]
  , [  5,  10,  10, -25, -25,   10,  10,   5]
  , [  0,   0,   0,   0,   0,   0,    0,   0]
  ]

blackPawnPieceSquareTable :: [[Int]]
blackPawnPieceSquareTable =
  [ [   0,  0,    0,   0,   0,   0,   0,   0]
  , [  -5, -10, -10,  25,  25, -10, -10,  -5]
  , [  -5,   5,  10,   0,   0,  10,   5,  -5]
  , [   0,   0,   0, -25, -25,   0,   0,   0]
  , [  -5,  -5, -10, -27, -27, -10,  -5,  -5]
  , [ -10, -10, -20, -30, -30, -20, -10, -10]
  , [ -50, -50, -50, -50, -50, -50, -50, -50]
  , [   0,   0,   0,   0,   0,   0,   0,   0]
  ]

pieceSquareTableLookup :: [[Int]] -> Coordinate -> Int
pieceSquareTableLookup t (Coordinate f r) = (t !! (r-1)) !! (fromEnum f - fromEnum 'a')

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
  pieceValue (Square (Just (Piece Pawn owner)) loc)   | owner == White = 100 + whitePawnLocValue loc
                                                      | otherwise      = -100 + blackPawnLocValue loc
  pieceValue (Square (Just (Piece Knight owner)) _)   | owner == White = 300
                                                      | otherwise      = -300
  pieceValue (Square (Just (Piece Bishop owner)) _)   | owner == White = 300
                                                      | otherwise      = -300
  pieceValue (Square (Just (Piece Rook owner)) _)     | owner == White = 500
                                                      | otherwise      = -500
  pieceValue (Square (Just (Piece Queen owner)) _)    | owner == White = 900
                                                      | otherwise      = -900
  pieceValue _ = 0

  whitePawnLocValue                    :: Coordinate -> Int
  whitePawnLocValue = pieceSquareTableLookup whitePawnPieceSquareTable

  blackPawnLocValue                    :: Coordinate -> Int
  blackPawnLocValue = pieceSquareTableLookup blackPawnPieceSquareTable

-- Inspired by John Hughes' "Why Functional Programming Matters"

gamesFrom      :: RegularGame -> [(Move, RegularGame)]
gamesFrom game = mapMaybe (\move -> (move,) <$> makeMoveFrom game move) (pseudoLegalMoves game)

gameTreeFrom          :: (RegularGame -> [RegularGame]) -> [Move] -> RegularGame -> GameTree RegularGame
gameTreeFrom f moves game = GameTree game (map (\edge -> (gameTreeFrom f ((fst edge):moves) (snd edge))) (gamesFrom game)) moves

maxByGameValue :: [GameTree RegularGame] -> GameTree RegularGame
maxByGameValue = maximumBy (comparing gameTreeValue)

minByGameValue :: [GameTree RegularGame] -> GameTree RegularGame
minByGameValue = minimumBy (comparing gameTreeValue)

minimize :: GameTree RegularGame -> GameTree RegularGame
minimize = minByGameValue . minimize'

minimize' :: GameTree RegularGame -> [GameTree RegularGame]
minimize' (GameTree v [] moves) = [GameTree v [] moves]
minimize' (GameTree _ children _) = mapMax (map maximize' children) where

  mapMax [] = []
  mapMax (x:xs) = (maxByGameValue x):(omit (maxByGameValue x) xs)

  omit pot [] = []
  omit pot (x:xs) | mingeq x pot = omit pot xs
                  | otherwise    = (maxByGameValue x):(omit (maxByGameValue x) xs)

  mingeq [] pot = False
  mingeq (x:xs) pot | (evalGame $ gameTreeValue x) >= (evalGame $ gameTreeValue pot) = True
                    | otherwise = mingeq xs pot

maximize :: GameTree RegularGame -> GameTree RegularGame
maximize = maxByGameValue . maximize'

maximize' :: GameTree RegularGame -> [GameTree RegularGame]
maximize' (GameTree v [] moves) = [GameTree v [] moves]
maximize' (GameTree _ children _) = mapMin (map minimize' children) where

  mapMin [] = []
  mapMin (x:xs) = (minByGameValue x):(omit (minByGameValue x) xs)

  omit pot [] = []
  omit pot (x:xs) | minleq x pot = omit pot xs
                  | otherwise    = (minByGameValue x):(omit (minByGameValue x) xs)

  minleq [] pot = False
  minleq (x:xs) pot | (evalGame $ gameTreeValue x) <= (evalGame $ gameTreeValue pot) = True
                    | otherwise = minleq xs pot

prune :: Int -> GameTree a -> GameTree a
prune 0 (GameTree v _ moves) = GameTree v [] moves
prune n (GameTree v children moves) = GameTree v (map (prune $ n - 1) children) moves

decideOnMove :: Player -> RegularGame -> Move
decideOnMove player game | player == White = head $ reverse $ gameTreeLastMove $ maximize $ prune 3 $ gameTreeFrom (mapMaybe (makeMoveFrom game) . pseudoLegalMoves) [] game
                         | otherwise = head $ reverse $ gameTreeLastMove $ minimize $ prune 3 $ gameTreeFrom (mapMaybe (makeMoveFrom game) . pseudoLegalMoves) [] game
