module Main (main) where
import MoveGen
import PrettyPrint
import ChessData
import Data.Tree

-- Should give us a list with the 20 possible positions after first half-move.
testGenMoves = generateMoves defaultGameState

gameTree = unfoldTree (\p -> ("some label", generateMoves p)) defaultGameState

boards = map board testGenMoves

main = putStrLn (foldl (\l r -> concat [l, pprintBoard r, "---\n"]) "" boards)