module Main (main) where
import MoveGen
import PrettyPrint
import ChessData
import Data.Tree

-- Should give us a list with the 20 possible positions after first half-move.
testGenMoves = generateMoves defaultGameState

makeNode state = (state, generateMoves state) 

gameTree = unfoldTree makeNode defaultGameState

boards = map board testGenMoves

prune :: Int -> Tree a -> Tree a
prune 0 (Node x ts) = Node x []
prune depth tree = prune (depth - 1) tree 

staticEval :: Board -> Double
staticEval b = sum (map squareValue (concat b))

squareValue :: Square -> Double
squareValue s = case s of
    Empty -> 0
    Full (Piece White p) -> pieceValue p
    Full (Piece Black p) -> negate (pieceValue p)

pieceValue :: PType -> Double  
pieceValue p = case p of
        Pawn -> 1
        Knight -> 3
        Bishop -> 3.3
        Rook -> 5
        Queen -> 9
        King -> 1000
        
maximise :: Tree Double -> Double 
maximise (Node n []) = n 
maximise (Node n subTree) = maximum (map minimise subTree)

minimise :: Tree Double -> Double 
minimise (Node n []) = n
minimise (Node n subTree) = minimum (map maximise subTree)

smallTree = prune 5 gameTree

smallTreeStaticEvaluated = fmap (staticEval .board) smallTree

eval = maximise smallTreeStaticEvaluated

main = print eval