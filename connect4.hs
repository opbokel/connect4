import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Maybe
import Text.Read

boardWidth = 7
boardHeight = 6
toWinLength = 4

data Status = Draw | Victory | Running deriving Eq

data Player = Player1 | Player2 deriving (Eq, Enum)

num :: Player -> Int
num player = (fromEnum player) + 1

instance Show Player where
    show player = "Player " ++ show (num player)

next :: Player -> Player
next Player1 = Player2
next Player2 = Player1

type Board = M.Matrix Int

type Coord = (Int, Int)

data State = State { player :: Player, board :: Board }

boardColIndex = M.fromList 1 boardWidth [1 .. boardWidth]

startBoard :: Board
startBoard = M.zero boardHeight boardWidth

isUsed value = value /= 0

validRowOrNothing :: Int -> Maybe Int
validRowOrNothing rowIndex = if (rowIndex < 1 || rowIndex > boardHeight) then Nothing else Just rowIndex 

nextFreeInColumn :: V.Vector Int -> Maybe Int
nextFreeInColumn column = validRowOrNothing (fromMaybe (V.length column) (V.findIndex isUsed column))

-- Matrix index starts at 1
nextFree :: Int -> Board -> Maybe Coord
nextFree colIndex board = 
    fmap (\rowIndex -> (rowIndex, colIndex)) ((M.safeGetCol colIndex board) >>= nextFreeInColumn)

placePiece :: Int -> State -> Maybe (State, Status)
placePiece colIndex state = do  
     coord     <- nextFree colIndex (board state)
     nextBoard <- M.safeSet (num $ player state) coord (board state)
     let status = getStatus coord (State (player state) nextBoard) -- Current player turn after placing the piece.
     return (State (next (player state)) nextBoard, status) 

checkWin :: Coord -> State -> Bool
checkWin coord state = False -- To be done

checkFull :: Board -> Bool
checkFull board = all isUsed (M.toList board)

getStatus :: Coord -> State -> Status
getStatus coord state 
    | checkWin coord state = Victory
    | checkFull (board state) = Draw
    | otherwise = Running

-- WARNING: Side effects beyond this point!

main :: IO ()
main = do
    putStrLn $ "Functional Connect " ++ show toWinLength ++ "\nAt any time, enter q to quit or r to restart:"
    play (State Player1 startBoard)                                
    
printBoard :: State -> IO ()
printBoard state = do 
    putStrLn $ show boardColIndex
    putStrLn $ show (board state)

play :: State -> IO ()
play state = do
    printBoard state
    putStrLn $ (show $ player state) ++ " Next move (1 to " ++ (show boardWidth) ++ ") ?"
    line <- getLine
    processLine line state

processLine :: [Char] -> State -> IO ()
processLine "r" _      = startNewGame
processLine "q" _      = putStrLn "The game will end now, have a very functional day!"
processLine line state = proccessColIndex (readMaybe line :: Maybe Int) state

startNewGame :: IO ()
startNewGame = do
    putStrLn "\nA new game will start!\n"
    main

proccessColIndex :: Maybe Int -> State -> IO ()
proccessColIndex Nothing state = do
    putStrLn "Invalid number, please try again."
    play state

proccessColIndex (Just colIndex) state = processNextState state (placePiece colIndex state)


processNextState :: State -> Maybe (State, Status) -> IO ()
processNextState currentState Nothing = do
    putStrLn "Invalid column number. Please verify and try again:"
    putStrLn  $ " - if your value is between 1 and " ++ (show boardWidth)
    putStrLn " - if the column is not full "
    play currentState

processNextState _ (Just (newState, Running)) = play newState

processNextState _ (Just (newState, Draw)) = do
    printBoard newState
    putStrLn "No losers today, the game is Draw!"
    startNewGame

processNextState currentState (Just (newState, Victory)) = do
    printBoard newState
    putStrLn $ (show $ player currentState) ++ " is the most functional player and the winner!"
    startNewGame






   
 