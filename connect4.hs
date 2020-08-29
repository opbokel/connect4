module Connect4
( Status(..)
, Player(..)
, State(..)
, Board(..)
, boardWidth
, boardHeight
, connectLength
, placePiece
, newGameState
, boardColIndex
) where

import qualified Data.Matrix as M
import Matrix
import qualified Data.Vector as V
import Data.Maybe

boardWidth = 7
boardHeight = 6
connectLength = 4

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

data State = State { player :: Player, board :: Board }

boardColIndex = M.fromList 1 boardWidth [1 .. boardWidth]

startBoard :: Board
startBoard = M.zero boardHeight boardWidth

newGameState = State Player1 startBoard

isUsed value = value /= 0

maybeRow :: Int -> Maybe Int
maybeRow rowIndex = if (rowIndex < 1 || rowIndex > boardHeight) then Nothing else Just rowIndex 

nextFreeInColumn :: V.Vector Int -> Maybe Int
nextFreeInColumn column = maybeRow (fromMaybe (V.length column) (V.findIndex isUsed column))

-- Matrix index starts at 1
nextFree :: Int -> Board -> Maybe Coord
nextFree colIndex board = do
    column   <- M.safeGetCol colIndex board
    rowIndex <- nextFreeInColumn column
    return (rowIndex, colIndex)

placePiece :: Int -> State -> Maybe (State, Status)
placePiece colIndex state = do  
     coord     <- nextFree colIndex (board state)
     nextBoard <- M.safeSet (num $ player state) coord (board state)
     let status = getStatus coord (State (player state) nextBoard) -- Current player turn after placing the piece.
     return (State (next (player state)) nextBoard, status)

isVictoryLine :: Coord  -> Coord -> State -> Bool
isVictoryLine startPoint direction state = 
    (walkWhileSame startPoint direction (board state)) + 1 
        + (walkWhileSame startPoint (inverseCoord direction) (board state))
        >= connectLength

checkVictory :: Coord -> State -> Bool
checkVictory startPoint state = 
    isVictoryLine startPoint (1,0) state
        || isVictoryLine startPoint (1,1) state
        || isVictoryLine startPoint (0,1) state
        || isVictoryLine startPoint (1,-1) state

checkFull :: Board -> Bool
checkFull board = all isUsed (M.toList board)

getStatus :: Coord -> State -> Status
getStatus coord state 
    | checkVictory coord state = Victory
    | checkFull (board state) = Draw
    | otherwise = Running
