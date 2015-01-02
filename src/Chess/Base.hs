module Chess.Base
  ( CastleRights(..),
    Coordinate(..),
    File,
    isOnBoard,
    Move(..),
    MoveType(..),
    Piece(..),
    PieceType(..),
    Player(..),
    Rank,
    RegularBoardRepresentation,
    RegularGame(..),
    Square(..),

    addPiece,
    coordinateEuclideanDistance,
    makeMove,
    offsetBy,
    rayFromMove,
    scaleBy,
    squareAt,
    unoccupied,
    unoccupiedByAlly
  ) where

import Control.Applicative
import Control.Monad.State.Lazy

import Data.Maybe

data Square = Square
  { pieceOn  :: Maybe Piece
  , location :: Coordinate
  } deriving(Eq, Read, Show)

data Piece = Piece
  { pieceType  :: PieceType
  , pieceOwner :: Player
  } deriving(Eq, Read)

data Move = Move
  { moveFrom     :: Coordinate
  , moveTo       :: Coordinate
  , moveType     :: MoveType
  } deriving(Eq, Read, Show)

data MoveType = Standard | Capture | Castle | Promotion | EnPassant deriving(Eq, Read, Show)

instance Show Piece where
  show (Piece Rook White)   = "R"
  show (Piece Knight White) = "N"
  show (Piece Bishop White) = "B"
  show (Piece Queen White)  = "Q"
  show (Piece King White)   = "K"
  show (Piece Pawn White)   = "P"

  show (Piece Rook Black)   = "r"
  show (Piece Knight Black) = "n"
  show (Piece Bishop Black) = "b"
  show (Piece Queen Black)  = "q"
  show (Piece King Black)   = "k"
  show (Piece Pawn Black)   = "p"

data PieceType  = Pawn | Knight | Bishop | Rook | Queen | King deriving (Enum, Eq, Read)
data Player = White | Black deriving (Enum, Eq, Read, Show)

data Coordinate = Coordinate File Rank deriving(Eq, Read, Show)
type Rank   = Int
type File   = Char

-- KkQq
data CastleRights = CastleRights Bool Bool Bool Bool deriving(Eq, Show)

type RegularBoardRepresentation   = [[Square]]

data RegularGame = RegularGame
  { placement       :: RegularBoardRepresentation
  , activeColor     :: Player
  , castlingRights  :: CastleRights
  , enPassantSquare :: Maybe Coordinate
  , halfMoveClock   :: Int
  , fullMoveNumber  :: Int
  } deriving(Eq)

instance Show RegularGame where
  show g =    "\n"
           ++ "  abcdefgh \n"
           ++ "8 " ++ concatMap (maybe "-" show) eighthRank  ++ " 8\n"
           ++ "7 " ++ concatMap (maybe "-" show) seventhRank ++ " 7\n"
           ++ "6 " ++ concatMap (maybe "-" show) sixthRank   ++ " 6\n"
           ++ "5 " ++ concatMap (maybe "-" show) fifthRank   ++ " 5\n"
           ++ "4 " ++ concatMap (maybe "-" show) fourthRank  ++ " 4\n"
           ++ "3 " ++ concatMap (maybe "-" show) thirdRank   ++ " 3\n"
           ++ "2 " ++ concatMap (maybe "-" show) secondRank  ++ " 2\n"
           ++ "1 " ++ concatMap (maybe "-" show) firstRank   ++ " 1\n"
           ++ "  abcdefgh \n "
           ++ "\n"
           ++ show (activeColor g) ++ " to move\n"
           ++ show ( castlingRights g) ++ "\n"
           ++ "En passant on " ++ show (enPassantSquare g) ++ "\n"
           ++ "Halfmove clock at " ++ show (halfMoveClock g) ++ "\n"
           ++ "Fullmove number " ++ show (fullMoveNumber g) ++ "\n" where
    eighthRank  = map pieceOn $ placement g !! 7
    seventhRank = map pieceOn $ placement g !! 6
    sixthRank   = map pieceOn $ placement g !! 5
    fifthRank   = map pieceOn $ placement g !! 4
    fourthRank  = map pieceOn $ placement g !! 3
    thirdRank   = map pieceOn $ placement g !! 2
    secondRank  = map pieceOn $ placement g !! 1
    firstRank   = map pieceOn $ head $ placement g

isOnBoard                  :: Coordinate -> Bool
isOnBoard (Coordinate f r) | f < 'a'   = False
                           | f > 'h'   = False
                           | r < 1     = False
                           | r > 8     = False
                           | otherwise = True

unoccupiedByAlly         :: RegularBoardRepresentation -> Coordinate -> Maybe Player -> Bool
unoccupiedByAlly b c ply | isNothing targetOwner = True
                         | ply /= targetOwner = True
                         | ply == targetOwner = False where
  targetPiece = pieceOn $ squareAt b c
  targetOwner = pieceOwner <$> targetPiece

unoccupied     :: RegularBoardRepresentation -> Coordinate -> Bool
unoccupied b c = isNothing . pieceOn $ squareAt b c

squareAt                    :: RegularBoardRepresentation -> Coordinate -> Square
squareAt b (Coordinate f r) = (b !! (r-1)) !! (fromEnum f - fromEnum 'a')

scaleBy                           :: Int -> (Int, Int) -> (Int, Int)
scaleBy s (x,y)                   = (x*s, y*s)

offsetBy                          :: Coordinate -> (Int, Int) -> Coordinate
offsetBy (Coordinate f r) (df,dr) = Coordinate (toEnum $ fromEnum f + df) (r + dr)

opponent               :: Player -> Player
opponent White         = Black
opponent Black         = White

coordinateEuclideanDistance                                       :: Coordinate -> Coordinate -> Int
coordinateEuclideanDistance (Coordinate cx y) (Coordinate cx' y') = ((x' - x) ^ 2) + ((y' - y) ^ 2) where
  x' = fromEnum cx' - fromEnum 'a'
  x  = fromEnum cx - fromEnum 'a'

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

pairEuclideanDistance               :: (Int, Int) -> (Int, Int) -> Int
pairEuclideanDistance (x,y) (x',y') = ((x' - x) ^ 2) + ((y' - y) ^ 2)

addPiece                        :: RegularBoardRepresentation -> Maybe Piece -> Coordinate -> RegularBoardRepresentation
addPiece b p c@(Coordinate f r) = newPlacement where
  newPlacement = fst splitBoard ++ [fst splitRank ++ [Square p c] ++ (tail . snd $ splitRank)] ++ (tail . snd $ splitBoard)
  splitBoard = splitAt (r - 1) b
  splitRank = splitAt (fromEnum f - fromEnum 'a') targetRank
  targetRank = head . snd $ splitBoard

updateSquare     :: Coordinate -> Maybe Piece -> State RegularGame ()
updateSquare c p = do
  game <- get
  let position = placement game
  put $ game { placement = addPiece position p c }

isLegal :: RegularGame -> Move -> Bool
isLegal game move@Move { moveFrom = from
                       , moveTo   = to
                       , moveType = movetype } = isQueenChecking where

  originalPiece = pieceOn $ squareAt (placement game) from

  nextState = addPiece (addPiece (placement game) Nothing from) originalPiece to

  activePly = fromJust $ pieceOwner <$> (pieceOn $ squareAt (placement game) from)

  isQueenChecking :: Bool
  isQueenChecking = null $ filter (\x -> ((== Capture) $ moveType x) && ((== Queen) . fromJust $ pieceType <$> (pieceOn . squareAt nextState $ moveTo x))) $ potentialQueenMoves nextState (location $ kingSquare activePly)

  kingSquare     :: Player -> Square
  kingSquare ply = head $ filter ((== Just (Piece King ply)) . pieceOn) $ foldr (++) [] nextState

makeStandardMove                              :: Move -> State RegularGame Bool
makeStandardMove move@Move { moveFrom = from
                           , moveTo   = to
                           , moveType = movetype } = do
  game <- get
  let position = placement game
  if (move `elem` pseudoLegalMoves game) && ((pieceOwner <$> (pieceOn $ (squareAt position from))) == (Just (activeColor game))) && (isLegal game move)
    then do let originalPiece = pieceOn $ squareAt position from
            put $ game { activeColor = opponent (activeColor game) }

            updateSquare from Nothing
            updateSquare to originalPiece
            return True
    else return False

disableWhiteCastles :: CastleRights -> CastleRights
disableWhiteCastles (CastleRights woo boo wooo booo) = CastleRights False boo False booo

disableBlackCastles :: CastleRights -> CastleRights
disableBlackCastles (CastleRights woo boo wooo booo) = CastleRights woo False wooo False

doCastle :: (CastleRights -> CastleRights) -> Player -> Move -> Move -> State RegularGame Bool
doCastle fupdaterights ply Move { moveFrom = from, moveTo = to } Move { moveFrom = rookfrom
                                                                      , moveTo   = rookto } = do
  game <- get
  let position = placement game
  let originalPiece = pieceOn $ squareAt position from
  put $ game { activeColor = opponent (activeColor game)
             , castlingRights = fupdaterights (castlingRights game)
             }

  updateSquare from Nothing
  updateSquare to originalPiece
  updateSquare rookfrom Nothing
  updateSquare rookto (Just $ Piece Rook ply)

  return True

makeWhiteKingsideCastle      :: Move -> State RegularGame Bool
makeWhiteKingsideCastle move = doCastle disableWhiteCastles White move
  Move { moveFrom = Coordinate 'h' 1
       , moveTo   = Coordinate 'f' 1
       , moveType = Castle
       }

makeWhiteQueensideCastle      :: Move -> State RegularGame Bool
makeWhiteQueensideCastle move = doCastle disableWhiteCastles White move
  Move { moveFrom = Coordinate 'a' 1
       , moveTo   = Coordinate 'd' 1
       , moveType = Castle
       }

makeBlackKingsideCastle      :: Move -> State RegularGame Bool
makeBlackKingsideCastle move = doCastle disableBlackCastles Black move
  Move { moveFrom = Coordinate 'h' 8
       , moveTo   = Coordinate 'f' 8
       , moveType = Castle
       }

makeBlackQueensideCastle      :: Move -> State RegularGame Bool
makeBlackQueensideCastle move = doCastle disableBlackCastles Black move
  Move { moveFrom = Coordinate 'a' 8
       , moveTo   = Coordinate 'd' 8
       , moveType = Castle
       }

makeCastle                              :: Move -> State RegularGame Bool
makeCastle move@Move { moveFrom = from
                     , moveTo   = to }  | from == Coordinate 'e' 1 && to == Coordinate 'g' 1 = makeWhiteKingsideCastle move
                                        | from == Coordinate 'e' 1 && to == Coordinate 'c' 1 = makeWhiteQueensideCastle move
                                        | from == Coordinate 'e' 8 && to == Coordinate 'g' 8 = makeBlackKingsideCastle move
                                        | from == Coordinate 'e' 8 && to == Coordinate 'c' 8 = makeBlackQueensideCastle move

makePromotion :: Maybe Piece -> Move -> State RegularGame Bool
makePromotion p@(Just Piece { pieceType  = pt, pieceOwner = o }) move@Move { moveFrom = from, moveTo = to } = do
  game <- get
  let position = placement game
  put $ game { activeColor = opponent (activeColor game) }

  updateSquare from Nothing
  updateSquare to p
  return True

makeEnPassant   :: Move -> State RegularGame Bool
makeEnPassant m@Move { moveTo = to@(Coordinate f r)
                     , moveFrom = from } = do
  game <- get
  if (m `elem` pseudoLegalMoves game)
    then do let position = placement game
            let originalPiece = pieceOn $ squareAt position from
            let rankOffset = if fmap pieceOwner originalPiece == Just White then (-1) else 1
            put $ game { activeColor = opponent (activeColor game) 
                       , enPassantSquare = Nothing }

            let enpassantpawn = pieceOn . squareAt position $ Coordinate f (r+rankOffset)

            updateSquare from Nothing
            updateSquare (Coordinate f (r+rankOffset)) Nothing
            updateSquare to originalPiece
            return True
    else return False


makeMove                                             :: Maybe Piece -> Move -> State RegularGame Bool
makeMove promoteTo move@Move { moveType = movetype } | movetype == Standard  = makeStandardMove move
                                                     | movetype == Capture   = makeStandardMove move
                                                     | movetype == Castle    = makeCastle move
                                                     | movetype == Promotion = makePromotion promoteTo move
                                                     | movetype == EnPassant = makeEnPassant move

pseudoLegalMoves               :: RegularGame -> [Move]
pseudoLegalMoves RegularGame { placement = b
                             , enPassantSquare = e
                             } = (concatMap . concatMap) (pseudoLegalMovesFrom e b) b where

pseudoLegalMovesFrom :: Maybe Coordinate -> RegularBoardRepresentation -> Square -> [Move]
pseudoLegalMovesFrom _ _ (Square Nothing _)            = []
pseudoLegalMovesFrom c b (Square (Just (Piece p _)) l) | p == Pawn   = potentialPawnMoves b c l
                                                       | p == Knight = potentialKnightMoves b l
                                                       | p == Bishop = potentialBishopMoves b l
                                                       | p == Rook   = potentialRookMoves b l
                                                       | p == Queen  = potentialQueenMoves b l
                                                       | p == King   = potentialKingMoves b (CastleRights True True True True) l

potentialPawnMoves                                       :: RegularBoardRepresentation -> Maybe Coordinate -> Coordinate -> [Move]
potentialPawnMoves b Nothing c                           = standardPawnMoves b c
potentialPawnMoves b (Just enPassant) c@(Coordinate r f) = standardPawnMoves b c ++ enPassantMoves enPassant where
  rankOffset :: Int
  rankOffset = case (fmap pieceOwner $ pieceOn $ squareAt b c) of
                 Just White -> 1
                 Just Black -> -1

  enPassantMoves                      :: Coordinate -> [Move]
  enPassantMoves (Coordinate r' f')   | (toEnum $ fromEnum r' + fromEnum rankOffset) == r
                                        && (f == (f' - 1) || f == (f' + 1))         = [Move { moveFrom = (Coordinate r f), moveTo = (Coordinate r' f'), moveType = EnPassant }]
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

advance                           :: RegularBoardRepresentation -> Coordinate -> Rank -> [Move]
advance b (Coordinate f r) offset | (unoccupied b $ Coordinate f (r+offset)) = [Move { moveFrom = (Coordinate f r), moveTo = (Coordinate f (r+offset)), moveType = Standard }]
                                  | otherwise                                = []

whiteCaptures                           :: RegularBoardRepresentation -> Coordinate -> [Move]
whiteCaptures b c@(Coordinate f _)      | f == 'a' = whiteNECapture b c
                                        | f == 'h' = whiteNWCapture b c
                                        | otherwise = whiteNWCapture b c ++ whiteNECapture b c

blackCaptures                           :: RegularBoardRepresentation -> Coordinate -> [Move]
blackCaptures b c@(Coordinate f _)      | f == 'a' = blackNECapture b c
                                        | f == 'h' = blackNWCapture b c
                                        | otherwise = blackNWCapture b c ++ blackNECapture b c

capture                                   :: RegularBoardRepresentation -> Coordinate -> File -> Rank -> Player -> [Move]
capture b (Coordinate f r) tf dr opponent | (isJust $ target) && fmap pieceOwner target == Just opponent = [Move { moveFrom = (Coordinate f r), moveTo = targetCoord, moveType = Capture }]
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
potentialRayMoves b c offsets = fmap (\x -> Move { moveFrom = c, moveTo = x, moveType = determineMoveType b c x }) $ filter isOnBoard $ fmap (c `offsetBy`) $ scaleBy <$> [1..7] <*> offsets where

determineMoveType                 :: RegularBoardRepresentation -> Coordinate -> Coordinate -> MoveType
determineMoveType b from to       | isNothing . pieceOn $ squareAt b to                          = Standard
                                  | (fmap pieceOwner . pieceOn $ squareAt b to) /= (fmap pieceOwner . pieceOn $ squareAt b from) = Capture

potentialBishopMoves     :: RegularBoardRepresentation -> Coordinate -> [Move]
potentialBishopMoves b c = filter (not . (isBlocked b)) $ potentialRayMoves b c diagonals where
  diagonals = [(-1,1),(1,1),(1,-1),(-1,-1)]

potentialRookMoves     :: RegularBoardRepresentation -> Coordinate -> [Move]
potentialRookMoves b c = filter (not . (isBlocked b)) $ potentialRayMoves b c straights where
  straights = [(1,0),(-1,0),(0,1),(0,-1)]

potentialQueenMoves     :: RegularBoardRepresentation -> Coordinate -> [Move]
potentialQueenMoves b c = potentialRookMoves b c ++ potentialBishopMoves b c

potentialOffsetMoves             :: RegularBoardRepresentation -> Coordinate -> [(Int, Int)] -> [Move]
potentialOffsetMoves b c offsets = fmap (\x -> Move { moveFrom = c, moveTo = x, moveType = determineMoveType b c x }) $ filter (flip (unoccupiedByAlly b) (fmap pieceOwner $ pieceOn $ squareAt b c)) $ filter isOnBoard $ fmap (c `offsetBy`) offsets

potentialKnightMoves     :: RegularBoardRepresentation -> Coordinate -> [Move]
potentialKnightMoves b c = potentialOffsetMoves b c possibleJumps where
  possibleJumps = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]

potentialKingMoves                                   :: RegularBoardRepresentation -> CastleRights -> Coordinate -> [Move]
potentialKingMoves b castlerights c@(Coordinate f r) | f == 'e' && r == 1 && (Just White) == kingOwner = potentialOffsetMoves b c possibleMoves ++ whiteCastles castlerights
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
  ooCastle homeRank ply | ooRookIsPresent homeRank ply && ooSquaresAreFree homeRank = [Move { moveFrom = Coordinate 'e' homeRank, moveTo = Coordinate 'g' homeRank, moveType = Castle }]
                        | otherwise = []

  ooRookIsPresent              :: Rank -> Player -> Bool
  ooRookIsPresent homeRank ply = (Just (Piece Rook ply)) == (pieceOn . squareAt b $ (Coordinate 'h' homeRank))

  ooSquaresAreFree          :: Rank -> Bool
  ooSquaresAreFree homeRank = all (unoccupied b) [(Coordinate 'f' homeRank), (Coordinate 'g' homeRank)]

  oooCastle              :: Rank -> Player -> [Move]
  oooCastle homeRank ply | oooRookIsPresent homeRank ply && oooSquaresAreFree homeRank = [Move { moveFrom = Coordinate 'e' homeRank, moveTo = Coordinate 'c' homeRank, moveType = Castle }]
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
