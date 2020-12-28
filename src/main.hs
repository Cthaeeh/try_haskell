module Main (main) where

import ChessData
import PrettyPrint

genMoves :: GameState -> [GameState]
genMoves gs = concatMap (movesFromSquare gs) (squaresOccupiedByColor (onMove gs) (board gs))

squaresOccupiedByColor :: Color -> Board -> [Sqr]
squaresOccupiedByColor color board = [x | x <- allSquares, hasPieceWithColor board color x]

allSquares :: [Sqr]
allSquares = cartesianProduct [0 .. 7] [0 .. 7]

-- TODO move to util
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

hasPieceWithColor :: Board -> Color -> Sqr -> Bool
hasPieceWithColor board color position = case board !! fst position !! snd position of
  Full (Piece color _) -> True
  x -> False

movesFromSquare :: GameState -> Sqr -> [GameState]
movesFromSquare gameState position = case board gameState !! fst position !! snd position of
  Empty -> []
  Full p -> makeMoves p position gameState

makeMoves :: Piece -> Sqr -> GameState -> [GameState]
makeMoves piece from gameState = map (makeMove gameState) (filter (isLegalMove gameState) (destinationSquares piece from))

makeMove :: GameState -> Move -> GameState 
makeMove gameState move = defaultGameState 

destinationSquares :: Piece -> Sqr -> [Move] 
destinationSquares piece from = case piece of
  Piece White Pawn -> [ (x, y) | x <- pawnMovesUp from, y <- repeat from]
  Piece Black Pawn -> [ (x, y) | x <- pawnMovesDown from, y <- repeat from]
  x -> []

isLegalMove :: GameState -> Move -> Bool
isLegalMove state move = not (isFriendlyFire (board state) move || isCollision state move || isSuicide state move || isPieceSpecificIllegal state move)

at :: Board -> Sqr -> Square 
at b (x, y) = b !! x !! y

pieceColor :: Board -> Sqr -> Maybe Color 
pieceColor board pos = case board `at` pos of
  Empty -> Nothing 
  Full p -> Just (color p)

-- You can not take your own pieces. 
isFriendlyFire :: Board -> Move -> Bool
isFriendlyFire board (to, from) = pieceColor board from == pieceColor board to

-- You can not jump over your own pieces (except knights). 
isCollision :: Board -> Move -> Bool
isCollision board (to, from) = 
  let nextX = x from + signum (x to - x from);
      nextY = y from + signum (y to - x to) in
  any [x y | x <- [(x from)..], y <- [(y from)..]] pieceColor 

-- You can make not get your self checked. 
isSuicide :: GameState -> Move -> Bool
isSuicide state move = False

-- Pawns can only capture diagonally, Castling rules must be valid, En passant must be valid.
isPieceSpecificIllegal :: GameState -> Move -> Bool
isPieceSpecificIllegal state move = False

--
--outSideBoard (a, b) = (0 > a) || (a > 7) || (0 > b) || (b > 7)

-- Generally possible rook moves from a position.
rookMoves (a, b) = zip [0 .. 7] (repeat a) ++ zip (repeat b) [0 .. 7]

-- Generally possible bishop moves from a position.
bishopMoves (a, b) = [(1, 0)]

-- Generally possible knight moves from a position.
knightMoves (a, b) = [(1, 0)]

-- Generally possible queen moves from a position.
queenMoves (a, b) = rookMoves (a, b) ++ bishopMoves (a, b)

-- Generally possible king moves from a position.
kingMoves (a, b) = [(a -1, b -1), (a -1, b), (a -1, b + 1), (a, b -1), (a, b + 1), (a + 1, b -1), (a + 1, b), (a + 1, b + 1)]

-- Generally possible Downwards Pawn moves from a position. -- TODO at start 2 squares
pawnMovesUp :: Sqr  -> [Sqr]
pawnMovesUp (a, b) = [(a, b + 1), (a - 1, b + 1), (a + 1, b + 1)]

-- Generally possible Upwards Pawn moves from a position. -- TODO at start 2 sqaures 
pawnMovesDown (a, b) = [(a, b + 1), (a - 1, b + 1), (a + 1, b + 1)]

-- Should print the usual chess starting position.
testPrint = pprintBoard (board defaultGameState)

-- Should give us a list with the 20 possible positions after first half-move.
testGenMoves = genMoves defaultGameState

main = putStrLn testPrint
