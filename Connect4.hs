module Connect4
( Status(..)
, Player(..)
, State(..)
, Board(..)
, boardWidth
, boardHeight
, connectLength
, newGameState
, placePiece
) where

import qualified Data.Matrix as M
import Matrix
import qualified Data.Vector as V
import Data.Maybe

boardWidth = 7
boardHeight = 6
connectLength = 4

data Status = Draw | Victory | Running deriving (Eq, Show)

data Player = Player1 | Player2 deriving (Eq, Enum)

num :: Player -> Int
num player = (fromEnum player) + 1

instance Show Player where
    show player = "Player " ++ show (num player)

next :: Player -> Player
next Player1 = Player2
next Player2 = Player1

type Board = M.Matrix Int

data State = State { player :: Player, board :: Board } deriving Show

startBoard :: Board
startBoard = M.zero boardHeight boardWidth

newGameState = State Player1 startBoard

isUsed value = value /= 0

placePiece :: Int -> State -> Maybe (State, Status)
placePiece colIndex (State player board) = do
    let findOrBottom = (fromMaybe (boardHeight + 1, colIndex)) . (findInColumnBy isUsed colIndex)
    let toPlaceCoord = (addCoord (-1, 0)) . findOrBottom $ board
    nextBoard        <- M.safeSet (num player) toPlaceCoord board
    let status       = getStatus toPlaceCoord nextBoard
    return (State (next player) nextBoard, status)


isVictoryLine :: Coord  -> Board -> Coord -> Bool
isVictoryLine startPoint board direction = 
    (walkWhileSame startPoint direction board) + 1 
        + (walkWhileSame startPoint (inverseCoord direction) board)
        >= connectLength

checkVictory :: Coord -> Board -> Bool
checkVictory startPoint board = 
    let isVictory = isVictoryLine startPoint board
    in isVictory (1,0) || isVictory (1,1) || isVictory (0,1) || isVictory (1,-1)

checkFull :: Board -> Bool
checkFull board = all isUsed (M.toList board)

getStatus :: Coord -> Board -> Status
getStatus coord board 
    | checkVictory coord board = Victory
    | checkFull board = Draw
    | otherwise = Running
