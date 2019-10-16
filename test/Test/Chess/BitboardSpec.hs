module Test.Chess.BitboardSpec where

import Chess.Base
import Chess.Bitboard

import Control.Applicative

import Data.Int
import Data.Word

import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.Placements
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.Util

rankAndFileIndices :: Gen (Int, Int)
rankAndFileIndices = do
  ri  <- ranks
  fi  <- files
  return (ri, fi)

ranks :: Gen Int
ranks = choose (0, 7)

files :: Gen Int
files = choose (0, 7)

diagonals :: Gen Int
diagonals = choose (-7, 7)

antiDiagonals :: Gen Int
antiDiagonals = choose (0, 14)

bitboards :: Gen Bitboard
bitboards = do
  w <- choose (minBound :: Word64, maxBound :: Word64)
  return $ Bitboard w

bitboardsAnd :: Gen a -> Gen (Bitboard, a)
bitboardsAnd gen = do
  bitboard <- bitboards
  a <- gen
  return (bitboard, a)

generatesAllTheSquaresIn :: (BoardIndex a) => (a -> Bitboard) -> (a -> [Int]) -> (a -> Bool)
generatesAllTheSquaresIn expected actual i = all ((isOccupied . expected) i) (actual i)

spec :: Spec
spec = describe "bitboard" $ do

  describe "integrations with Base" $ do
    it "can convert Coordinates to 0-based (rank, file) indices" $ do
      forAll coords (\c@(Coordinate f r) -> coordinateToIndices c == (r - 1, fromEnum f - 97))

    it "can convert 0-based (rank, file) indices to Coordinates" $ do
      forAll rankAndFileIndices (\(r, f) -> indicesToCoordinate (r, f) == (Coordinate (toEnum $ f + 97) (r + 1)))

    it "can convert (rank, file) indices to a single square index from [0..63]" $ do
      forAll rankAndFileIndices (\(r, f) -> indicesToSquareIndex (r, f) == 8 * r + f)

    it "can convert single square indices to (rank, file) indices" $ do
      forAll rankAndFileIndices (\(r, f) -> squareIndexToIndices (8 * r + f) == (r, f))

  describe "formatting" $ do
    it "is showable" $ do
      show (Bitboard 9820426766351346249) `shouldBe` "\n. . . 1 . . . 1\n1 . . 1 . . 1 .\n. 1 . 1 . 1 . .\n. . 1 1 1 . . .\n1 1 1 . 1 1 1 1\n. . 1 1 1 . . .\n. 1 . 1 . 1 . .\n1 . . 1 . . 1 .\n9820426766351346249"

  describe "representation" $ do
    {--
    . . . . . . . 1
    . . . . . . 1 .
    . . . . . 1 . .
    . . . . 1 . . .
    . . . 1 . . . .
    . . 1 . . . . .
    . 1 . . . . . .
    1 . . . . . . .
    --}
    it "uses little-endian rank-file mapping" $ do
      isOccupied (Bitboard 9241421688590303745) (0 :: Int) && isOccupied (Bitboard 9241421688590303745) (63 :: Int) `shouldBe` True

    it "has a squareIndex defined in terms of a rankIndex and a fileIndex" $ do
      forAll (bitboardsAnd rankAndFileIndices) $ (\(bitboard, (ri, fi)) -> (isOccupied bitboard $ 8 * ri + fi) == isOccupied bitboard (ri, fi))

    it "can be indexed by Coordinate" $ do
      forAll (bitboardsAnd coords) $ (\(bitboard, c@(Coordinate f r)) -> (isOccupied bitboard c) == isOccupied bitboard (coordinateToIndices c))

  describe "setwise operations" $ do
    {--
    . . . 1 . . . 1     . . . . . . . .     . . . . . . . .
    1 . . 1 . . 1 .     1 1 1 1 1 1 1 1     1 . . 1 . . 1 .
    . 1 . 1 . 1 . .     . . . . . . . .     . . . . . . . .
    . . 1 1 1 . . .     . . . . . . . .     . . . . . . . .
    1 1 1 * 1 1 1 1  &  . . . * . . . .  =  . . . * . . . .
    . . 1 1 1 . . .     . . . . . . . .     . . . . . . . .
    . 1 . 1 . 1 . .     . . . . . . . .     . . . . . . . .
    1 . . 1 . . 1 .     . . . . . . . .     . . . . . . . .
    --}

    it "include intersection" $ do
      let allMoves       = Bitboard 9820426766351346249
      let enemyPieces    = Bitboard 71776119061217280
      let attackedPieces = Bitboard 20547673299877888
      (allMoves `bitboardIntersect` enemyPieces) `shouldBe` attackedPieces

    {--
    . . . 1 . . . .     . . . . . . . 1     . . . 1 . . . 1
    . . . 1 . . . .     1 . . . . . 1 .     1 . . 1 . . 1 .
    . . . 1 . . . .     . 1 . . . 1 . .     . 1 . 1 . 1 . .
    . . . 1 . . . .     . . 1 . 1 . . .     . . 1 1 1 . . .
    1 1 1 * 1 1 1 1  |  . . . * . . . .  =  1 1 1 * 1 1 1 1
    . . . 1 . . . .     . . 1 . 1 . . .     . . 1 1 1 . . .
    . . . 1 . . . .     . 1 . . . 1 . .     . 1 . 1 . 1 . .
    . . . 1 . . . .     1 . . . . . 1 .     1 . . 1 . . 1 .

    0000100000001000000010000000100011110111000010000000100000001000
    --}

    it "include union" $ do
      let rookMoves   = Bitboard 578721386714368008
      let bishopMoves = Bitboard 9241705379636978241
      let queenMoves  = Bitboard 9820426766351346249
      (rookMoves `bitboardUnion` bishopMoves) `shouldBe` queenMoves

    it "include inversion or complement" $ do
      let startingPosTotalOccupancy = Bitboard 18446462598732906495
      bitboardComplement startingPosTotalOccupancy `shouldBe` Bitboard 281474976645120

  describe "line attacks" $ do
    it "can calculate line attacks for any given rank" $ do
      forAll ranks $ rankMask `generatesAllTheSquaresIn` (\rank -> map (\i -> 8 * rank + i) [0..7])

    it "can calculate line attacks for any given file" $ do
      forAll files $ fileMask `generatesAllTheSquaresIn`(\file -> map (\offset -> file + 8 * offset) [0..7])

    it "can calculate line attacks for any given diagonal" $ do
      forAll diagonals $ diagonalMask `generatesAllTheSquaresIn` (\diagonal -> map (\offset -> if diagonal >= 0
        then 8 * diagonal + 9 * offset
        else (-1) * diagonal + 9 * offset) [0..(7 - abs diagonal)])

    it "can calculate line attacks for any given antidiagonal" $ do
      forAll antiDiagonals $ antiDiagonalMask `generatesAllTheSquaresIn` (\d -> map (\offset -> if d <= 7 then (8 * d) - (7 * offset) else ((56 + (d - 7)) - (7 * offset))) [0.. 7 - (abs (d - 7))])

  describe "ray attacks" $ do
    describe "unobstructed rays" $ do
      it "can calculate the north ray attack starting from an origin square" $ do
        forAll rankAndFileIndices $ northRay `generatesAllTheSquaresIn` (\(rank, file) -> map (\offset -> (indicesToSquareIndex (rank, file)) + 8 * offset) [1..7-rank])

      it "can calculate the south ray attack starting from an origin square" $ do
        forAll rankAndFileIndices $ southRay `generatesAllTheSquaresIn` (\(rank, file) -> map (\offset -> (indicesToSquareIndex (rank, file)) - 8 * offset) [1..rank])

      it "can calculate the east ray attack starting from an origin square" $ do
        forAll rankAndFileIndices $ eastRay `generatesAllTheSquaresIn` (\(rank, file) -> map (\offset -> (indicesToSquareIndex (rank, file)) + offset) [1..7-file])

      it "can calculate the west ray attack starting from an origin square" $ do
        forAll rankAndFileIndices $ westRay `generatesAllTheSquaresIn` (\(rank, file) -> map (\offset -> (indicesToSquareIndex (rank, file)) - offset) [1..file])

      it "can calculate the northeast ray attack starting from an origin square" $ do
        forAll rankAndFileIndices $ northEastRay `generatesAllTheSquaresIn` (\(rank, file) ->
          let diagonal = (rank - file)
              endingSquare = if diagonal >= 0
                               then 63 - (7 - file)
                               else 63 - 8 * (7 - rank) in
          map (\offset -> (indicesToSquareIndex (rank, file) + 9 * offset)) [1..(endingSquare - indicesToSquareIndex (rank, file)) `div` 9])

      it "can calculate the southeast ray attack starting from an origin square" $ do
        forAll rankAndFileIndices $ southEastRay `generatesAllTheSquaresIn` (\(rank, file) ->
          let antidiagonal = (rank + file)
              endingSquare = if antidiagonal >= 7
                               then 7 + 8 * (antidiagonal - 7)
                               else antidiagonal in
          map (\offset -> (indicesToSquareIndex (rank, file) - 7 * offset)) [1..(indicesToSquareIndex (rank, file) - endingSquare) `div` 7])

      it "can calculate the southwest ray attack starting from an origin square" $ do
        forAll rankAndFileIndices $ southWestRay `generatesAllTheSquaresIn` (\(rank, file) ->
          let diagonal = (rank - file)
              endingSquare = if diagonal >= 0
                               then 8 * diagonal
                               else (-1) * diagonal in
          map (\offset -> (indicesToSquareIndex (rank, file) - 9 * offset)) [1..(indicesToSquareIndex (rank, file) - endingSquare) `div` 9])

      it "can calculate the northwest ray attack starting from an origin square" $ do
        forAll rankAndFileIndices $ northWestRay `generatesAllTheSquaresIn` (\(rank, file) ->
          let antidiagonal = (rank + file)
              endingSquare = if antidiagonal >= 7
                               then 56 + (antidiagonal - 7)
                               else 56 - 8 * (7 - antidiagonal) in
          map (\offset -> (indicesToSquareIndex (rank, file) - 9 * offset)) [1..(indicesToSquareIndex (rank, file) - endingSquare) `div` 9])

  describe "translations" $ do
    it "can translate bitboards in the north direction" $ do
      {--
      . . . 1 . . . 1     1 . . 1 . . 1 .
      1 . . 1 . . 1 .     . 1 . 1 . 1 . .
      . 1 . 1 . 1 . .     . . 1 1 1 . . .
      . . 1 1 1 . . .     1 1 1 * 1 1 1 1
      1 1 1 * 1 1 1 1 =>  . . 1 1 1 . . .
      . . 1 1 1 . . .     . 1 . 1 . 1 . .
      . 1 . 1 . 1 . .     1 . . 1 . . 1 .
      1 . . 1 . . 1 .     . . . 1 . . . 1
      --}

      translateNorth (Bitboard 9820426766351346249) `shouldBe` Bitboard 5272058161445620104

  describe "conversion to regular board representations" $ do
    it "can return a list of coordinates represented by the bitboard" $ do
      forAll bitboards (\bitboard ->
        (all (\coordinate -> isOccupied bitboard coordinate) $ bitboardToCoordinates bitboard) &&
        (all (\coordinate -> not $ isOccupied bitboard coordinate) $ bitboardToCoordinates $ bitboardComplement bitboard))

  describe "conversion from regular board representations" $ do

    it "can produce an occupancy bitboard for white pawns" $ do
      whitePawnOccupancyFor (placement startingPos) `shouldBe` Bitboard 65280

    it "can produce an occupancy bitboard for black pawns" $ do
      blackPawnOccupancyFor (placement startingPos) `shouldBe` Bitboard 71776119061217280

    it "can produce an occupancy bitboard for white knights" $ do
      whiteKnightOccupancyFor (placement startingPos) `shouldBe` Bitboard 66

    it "can produce an occupancy bitboard for black knights" $ do
      blackKnightOccupancyFor (placement startingPos) `shouldBe` Bitboard 4755801206503243776

    it "can produce an occupancy bitboard for white bishops" $ do
      whiteBishopOccupancyFor (placement startingPos) `shouldBe` Bitboard 36

    it "can produce an occupancy bitboard for black bishops" $ do
      blackBishopOccupancyFor (placement startingPos) `shouldBe` Bitboard 2594073385365405696

    it "can produce an occupancy bitboard for white rooks" $ do
      whiteRookOccupancyFor (placement startingPos) `shouldBe` Bitboard 129

    it "can produce an occupancy bitboard for black rooks" $ do
      blackRookOccupancyFor (placement startingPos) `shouldBe` Bitboard 9295429630892703744

    it "can produce an occupancy bitboard for white queens" $ do
      whiteQueenOccupancyFor (placement startingPos) `shouldBe` Bitboard 8

    it "can produce an occupancy bitboard for black queens" $ do
      blackQueenOccupancyFor (placement startingPos) `shouldBe` Bitboard 576460752303423488

    it "can produce an occupancy bitboard for white kings" $ do
      whiteKingOccupancyFor (placement startingPos) `shouldBe` Bitboard 16

    it "can produce an occupancy bitboard for black kings" $ do
      blackKingOccupancyFor (placement startingPos) `shouldBe` Bitboard 1152921504606846976

    it "can produce the total occupancy of the board, for white pieces" $ do
      whiteOccupancyFor (placement startingPos) `shouldBe` Bitboard 65535

    it "can produce the total occupancy of the board, for both colours" $ do
      totalOccupancyFor (placement startingPos) `shouldBe` Bitboard 18446462598732906495

    it "can convert a RegularBoardRepresentation into a BitboardRepresentation" $ do
      regularToBitboard (placement startingPos) `shouldBe` BitboardRepresentation
        { whitePawns   = Bitboard 65280
        , blackPawns   = Bitboard 71776119061217280
        , whiteKnights = Bitboard 66
        , blackKnights = Bitboard 4755801206503243776
        , whiteBishops = Bitboard 36
        , blackBishops = Bitboard 2594073385365405696
        , whiteRooks   = Bitboard 129
        , blackRooks   = Bitboard 9295429630892703744
        , whiteQueens  = Bitboard 8
        , blackQueens  = Bitboard 576460752303423488
        , whiteKings   = Bitboard 16
        , blackKings   = Bitboard 1152921504606846976
        }

    modifyMaxSuccess (const $ 2 ^ 16) $ describe "bitscan" $ do

      describe "bitscan forward" $ do
        it "can find the least significant one bit in a Bitboard" $ do
          forAll bitboards (\bitboard -> bitscanForward bitboard == minimum (filter (isOccupied bitboard) [0..63]))

      describe "bitscan reverse" $ do
        it "can find the most significant one bit in a Bitboard" $ do
          forAll bitboards (\bitboard -> bitscanReverse bitboard == maximum (filter (isOccupied bitboard) [0..63]))
