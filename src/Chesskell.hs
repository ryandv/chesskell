{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson.Encode
import Data.Aeson.Types

import Chess.AI
import Chess.Base
import Chess.FenParser
import Chess.Game
import Chess.MoveGen

import Control.Monad
import Control.Monad.Trans

import qualified Data.ByteString.Char8 as C
import Data.Maybe

import Happstack.Server
import Happstack.Server.Types

import System.Environment

import Text.Parsec

data JsonMove = JsonMove
  { fenString :: String
  , jsonMoveFrom  :: String
  , jsonMoveTo    :: String
  }

instance ToJSON JsonMove where
  toJSON (JsonMove fen from to) = object ["fen" .= fen, "from" .= from, "to" .= to]

coordPairToMove :: RegularGame -> Coordinate -> Coordinate -> [Move]
coordPairToMove game from to = filter (\move -> (moveFrom move == from) && (moveTo move == to)) $ pseudoLegalMoves game

requestMoveHandler :: ServerPart Response
requestMoveHandler = do

  req <- askRq

  decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)

  fenString <- look "fen"

  let position = either (const undefined) id $ parseFen "" fenString

  let chosenAIMove = decideOnMove (activeColor position) position
  let chosenAIMoveFrom = case (moveFrom chosenAIMove) of
                           Coordinate f r -> return f ++ show r
  let chosenAIMoveTo = case (moveTo chosenAIMove) of
                           Coordinate f r -> return f ++ show r

  let positionAfterAIMove = fromJust $ makeMoveFrom position chosenAIMove

  ok (toResponseBS (C.pack "application/json") $ encode (JsonMove (toFEN positionAfterAIMove) chosenAIMoveFrom chosenAIMoveTo))

makeMoveHandler :: ServerPart Response
makeMoveHandler = do

  req <- askRq

  decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)

  fenString <- look "fen"
  jsonMoveFrom  <- look "from"
  jsonMoveTo    <- look "to"

  let position = either (const undefined) id $ parseFen "" fenString

  let fromCoord = either (const $ Coordinate 'a' 1) fromJust $ runParser enPassantSquareParser (FenParserState $ Coordinate 'a' 8) "" (jsonMoveFrom ++ " ")
  let toCoord = either (const $ Coordinate 'a' 1) fromJust $ runParser enPassantSquareParser (FenParserState $ Coordinate 'a' 8) "" (jsonMoveTo ++ " ")

  let plyMove = head (coordPairToMove position fromCoord toCoord)
  let plyMoveFrom = case (moveFrom plyMove) of
                           Coordinate f r -> return f ++ show r
  let plyMoveTo = case (moveTo plyMove) of
                           Coordinate f r -> return f ++ show r

  let positionAfterPlayerMove = makeMoveFrom position plyMove

  let response = case positionAfterPlayerMove of
                   Nothing -> ok (toResponseBS (C.pack "application/json") $ encode (JsonMove (toFEN position) plyMoveFrom plyMoveTo))
                   Just newPosition -> ok (toResponseBS (C.pack "application/json") $ encode (JsonMove (toFEN newPosition) "" ""))

  response


main :: IO ()
main = do
  envPort <- getEnv "PORT"
  simpleHTTP (nullConf { port = read envPort })
    $ msum [ dir "makemove" $ do method POST
                                 makeMoveHandler
           , dir "requestmove" $ do method POST
                                    requestMoveHandler
           , dir "js" $ serveDirectory DisableBrowsing ["index.html"] "cheskell/js"
           , dir "css" $ serveDirectory DisableBrowsing ["index.html"] "cheskell/css"
           , serveFile (guessContentTypeM mimeTypes) "cheskell/index.html"
           ]
