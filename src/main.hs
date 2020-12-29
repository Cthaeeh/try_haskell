module Main (main) where

import ChessData
import PrettyPrint

count pred list = sum [ 1 | x <- list, pred x ]

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
hasPieceWithColor board color position =  pieceColor board position == Just color

movesFromSquare :: GameState -> Sqr -> [GameState]
movesFromSquare gameState position = case board gameState `at` position of
  Empty -> []
  Full p -> movesWithPieceFrom p position gameState

movesWithPieceFrom :: Piece -> Sqr -> GameState -> [GameState]
movesWithPieceFrom piece from gameState = map (transformGameState gameState) (filter (isLegalMove gameState) (genericMoves piece from))

next c = case c of
  White -> Black
  Black -> White

-- TODO castling, en passant
transformGameState :: GameState -> Move -> GameState 
transformGameState state move = GameState (next (onMove state)) (makeMove (board state) move) defaultCastlingRights False

-- TODO promotion 
makeMove :: Board -> Move -> Board 
makeMove board (from, to) = replace (replace board to (board `at` to)) from Empty 

replace :: Board -> Sqr -> Square -> Board
replace board (x, y) content = changeNth x (changeNth y content (board !! x)) board

changeNth :: Int->a->[a]->[a]
changeNth n e (x:xs)
     | n == 0 = e:xs
     | otherwise = x:changeNth (n-1) e xs

genericMoves :: Piece -> Sqr -> [Move] 
genericMoves piece from = case piece of
  Piece White Pawn -> zip (repeat from) (pawnMovesUp from) 
  Piece Black Pawn -> zip (repeat from) (pawnMovesDown from) 
  x -> []

isLegalMove :: GameState -> Move -> Bool
isLegalMove state move = not (isFriendlyFire (board state) move || isCollision (board state) move || isSuicide state move || isPieceSpecificIllegal state move)

at :: Board -> Sqr -> Square 
at b (x, y) = b !! y !! x



pieceColor :: Board -> Sqr -> Maybe Color 
pieceColor board pos = case board `at` pos of
  Empty -> Nothing 
  Full p -> Just (color p)

-- You can not take your own pieces. 
isFriendlyFire :: Board -> Move -> Bool
isFriendlyFire board (to, from) = pieceColor board from == pieceColor board to

x :: (a, b) -> a
x = fst
y :: (a, b) -> b
y = snd

isFull x = case x of
  Empty -> False
  x -> True

-- You can not jump over your own pieces (except knights). 
isCollision :: Board -> Move -> Bool
isCollision board (to, from) = 
    let nextX = x from + signum (x to - x from);
        nextY = y from + signum (y to - x to) in
    count isFull [board `at` (x, y) | x <- [nextX .. x to], y <- [nextY.. y to]] >= 2
    

-- You can make not get your self checked. 
isSuicide :: GameState -> Move -> Bool
isSuicide state move = False

-- Pawns can only capture diagonally, Castling rules must be valid, En passant must be valid.
isPieceSpecificIllegal :: GameState -> Move -> Bool
isPieceSpecificIllegal state move = False

--
outside :: Sqr -> Bool
outside (x, y) = (0 > x) || (x > 7) || (0 > y) || (y > 7)

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

-- Generally possible Upwards Pawn moves from a position. (white pawns)
pawnMovesUp :: Sqr  -> [Sqr]
pawnMovesUp (x, y) = [x | x <- [(x, y + 1), (x + 1, y + 1), (x - 1, y + 1)], not (outside x)] ++ [(x, y + 2) | y == 1]


-- Generally possible Downwards Pawn moves from a position. (black pawns)
pawnMovesDown :: Sqr  -> [Sqr]
pawnMovesDown (x, y) = [x | x <- [(x, y - 1), (x - 1, y - 1), (x + 1, y - 1)], not (outside x)] ++ [(x, y - 2) | y == 6]

-- Should print the usual chess starting position.
testPrint = pprintBoard (board defaultGameState)

-- Should give us a list with the 20 possible positions after first half-move.
testGenMoves = genMoves defaultGameState

boards = map board testGenMoves

main = putStrLn (foldl (\l r -> concat [l, pprintBoard r, "---\n"]) "" boards)

--main = print (show (genericMoves (Piece White Pawn) (1,1)))

