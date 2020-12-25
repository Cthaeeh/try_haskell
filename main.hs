data Color = White | Black deriving (Show)
data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show)

data Piece = Piece { color     ::   Color ,
                     pieceType :: PType
                   } deriving (Show)

data Square = Empty | Full Piece deriving (Show)
type Board = [[Square]]

pprintSquare :: Square -> String
pprintSquare s = case s of
    Empty -> " "
    Full (Piece White King) -> "K"
    Full (Piece White Queen) -> "Q"
    Full (Piece White Rook) -> "R"
    Full (Piece White Knight) -> "N"
    Full (Piece White Bishop) -> "B"
    Full (Piece White Pawn) -> "P"
    Full (Piece Black King) -> "k"
    Full (Piece Black Queen) -> "q"
    Full (Piece Black Rook) -> "r"
    Full (Piece Black Knight) -> "n"
    Full (Piece Black Bishop) -> "b"
    Full (Piece Black Pawn) -> "p"

pprintLine :: [Square] -> String
pprintLine l = foldl (\l r -> concat [l, " ", pprintSquare r]) "" l

pprintBoard :: Board -> String
pprintBoard b = foldl (\l r -> concat [l, pprintLine r, "\n"]) "" b

eM = Empty

wK = Full (Piece White King)
wQ = Full (Piece White Queen)
wR = Full (Piece White Rook)
wB = Full (Piece White Bishop)
wN = Full (Piece White Knight)
wP = Full (Piece White Pawn)

bK = Full (Piece Black King)
bQ = Full (Piece Black Queen)
bR = Full (Piece Black Rook)
bB = Full (Piece Black Bishop)
bN = Full (Piece Black Knight)
bP = Full (Piece Black Pawn)

defaultBoard = [[bR,bN,bB,bQ,bK,bB,bN,bR],
                [bP,bP,bP,bP,bP,bP,bP,bP],
                [eM,eM,eM,eM,eM,eM,eM,eM],
                [eM,eM,eM,eM,eM,eM,eM,eM],
                [eM,eM,eM,eM,eM,eM,eM,eM],
                [eM,eM,eM,eM,eM,eM,eM,eM],
                [wP,wP,wP,wP,wP,wP,wP,wP],
                [wR,wN,wB,wQ,wK,wB,wN,wR]]

type EnPassantRights = Bool
data CastlingRights = CastlingRights {whiteOO  :: Bool,
                                      whiteOOO :: Bool,
                                      blackOO  :: Bool,
                                      blackOOO :: Bool} deriving (Show)

defaultCastlingRights = CastlingRights False False False False

data GameState = GameState {onMove          :: Color,
                            board           :: Board,
                            castlingRights  :: CastlingRights,
                            enPassantRights :: EnPassantRights} deriving (Show)

defaultGameState = GameState{onMove = White,
                             board = defaultBoard,
                             castlingRights = defaultCastlingRights,
                             enPassantRights = False}

d = pprintBoard (board defaultGameState )

genMoves :: GameState -> [GameState]
genMoves gs = fmap (squaresOccupiedByColor (onMove gs) (board gs)) (movesFrom gs)

squaresOccupiedByColor color board = [x | x <- allPositions, hasPieceWithColor board color x]

allPositions = cartesianProduct [0..7] [0..7]

cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

hasPieceWithColor board color position = case (board[fst position][snd position]) of
        Full (Piece color _) -> True
        x -> False

movesFrom gameState position = case ((board gamestate)[fst position][snd position]) of 
        x -> []
        Full p -> makeMoves p position gameState

makeMoves piece position gameState = ???

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

main = putStrLn (show (rookMoves (0, 0)) )


