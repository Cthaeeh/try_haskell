module Main (main) where
import ChessData 
import PrettyPrint

genMoves :: GameState -> [GameState]
genMoves gs = fmap (squaresOccupiedByColor (onMove gs) (board gs)) (movesFrom gs)

squaresOccupiedByColor :: Color -> Board -> [Pos]
squaresOccupiedByColor color board = [x | x <- allPositions, hasPieceWithColor board color x]

allPositions = cartesianProduct [0..7] [0..7]

cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

hasPieceWithColor :: Board -> Color -> Pos -> Bool
hasPieceWithColor board color position = case (board!!(fst position)!!(snd position)) of
        Full (Piece color _) -> True
        x -> False

movesFrom :: GameState -> Pos -> [GameState]
movesFrom gameState position = case ((board gameState)!!(fst position)!!(snd position)) of 
        x -> []
        Full p -> makeMoves p position gameState

makeMoves :: Piece -> Pos -> GameState -> [GameState]
makeMoves piece from gameState = filter (isLegalMove gameState from) (map (makeMove gameState from) (possibleMoves piece from) )

makeMove :: GameState -> Pos -> Pos -> GameState
makeMove gs from to = gs 

possibleMoves ::  Piece -> Pos -> [Pos]
possibleMoves piece pos = case piece of 
        Piece White Pawn -> pawnMovesUp pos
        Piece Black Pawn -> pawnMovesDown pos
        Piece White Knight  -> knightMoves pos
        Piece Black Knight  -> knightMoves pos
        Piece White Bishop  -> bishopMoves pos
        Piece Black Bishop  -> bishopMoves pos
        Piece White Rook  -> rookMoves pos
        Piece White Rook  -> rookMoves pos
        Piece Black Queen  -> queenMoves pos
        Piece Black Queen  -> queenMoves pos
        Piece Black King  -> kingMoves pos
        Piece Black King  -> kingMoves pos

isLegalMove :: GameState -> Pos -> Pos -> Bool
isLegalMove gs from to = not (isFriendlyFire gs from to) && not (isCollision gs from to) && not (isSuicide gs from to)

isFriendlyFire :: GameState -> Pos -> Pos -> Bool
isFriendlyFire gs from to = False 

isCollision :: GameState -> Pos -> Pos -> Bool
isCollision gs from to = False 

isSuicide :: GameState -> Pos -> Pos-> Bool
isSuicide gs from to = False 
        
outSideBoard (a,b) = (0 > a) || (a > 7) || (0 > b) || (b > 7)
-- Generally possible rook moves from a position.
rookMoves (a,b) = zip [0..7] (repeat a) ++ zip (repeat b) [0..7]
-- Generally possible bishop moves from a position.
bishopMoves (a,b) = [(1,0)]
-- Generally possible knight moves from a position.
knightMoves (a,b) = [(1,0)]
-- Generally possible queen moves from a position.
queenMoves (a,b) = rookMoves (a,b) ++ bishopMoves (a,b)
-- Generally possible king moves from a position.
kingMoves (a,b) = [(a-1,b-1),(a-1,b),(a-1,b+1), (a,b-1),(a,b+1), (a+1, b-1),(a+1,b),(a+1,b+1)]
-- Generally possible Downwards Pawn moves from a position.
pawnMovesUp (a,b) = [x | x <- [(a,b+1),(a,b-1),(a,b-1)]]
-- Generally possible Upwards Pawn moves from a position.
pawnMovesDown (a,b) = [x | x <- [(a,b+1),(a,b-1),(a,b-1)]]

-- Should print the usual chess starting position.
testPrint = pprintBoard (board defaultGameState)

-- Should give us a list with the 20 possible positions after first half-move.
testGenMoves = genMoves defaultGameState


main = putStrLn testPrint 
