import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Maybe

boardWidth = 7
boardHeight = 6
toWinLength = 4

data Status = Draw | Victory | Running deriving Eq

data Player = Player1 | Player2 deriving Eq

num :: Player -> Int
num Player1 = 1
num Player2 = 2

instance Show Player where
    show player = "Player " ++ show (num player)

next :: Player -> Player
next Player1 = Player2
next Player2 = Player1

type Board = M.Matrix Int

type Coord = (Int, Int)

startBoard :: Board
startBoard = M.zero boardHeight boardWidth

boardColIndex = M.fromList 1 boardWidth [1 .. boardWidth]

isUsed value = value /= 0

validOrNothing 0 = Nothing
validOrNothing colIndex = Just colIndex

nextFreeInColumn :: V.Vector Int -> Maybe Int
nextFreeInColumn column = validOrNothing (fromMaybe (V.length column) (V.findIndex isUsed column))

-- Matrix index starts at 1
nextFree :: Int -> Board -> Maybe Coord
nextFree colIndex board = 
    fmap (\rowIndex -> (rowIndex, colIndex)) ((M.safeGetCol colIndex board) >>= nextFreeInColumn)

placePiece :: Player -> Int -> Board -> Maybe (Board, Coord)
placePiece player colIndex board = do  
     coord  <- nextFree colIndex board
     matrix <- M.safeSet (num player) coord board
     return (matrix, coord) 

checkWin :: Player -> Coord -> Board -> Bool
checkWin player coord board = False -- To be done

main :: IO ()
main = do
    putStrLn ("Functional Connect " ++ show toWinLength ++ "\n At any time, enter q to quit or r to restart")
    play Player1 startBoard                                
    

play :: Player -> Board -> IO ()
play player board = do
    putStrLn (show boardColIndex)
    putStrLn (show board)
    putStrLn ((show player) ++ " next move (1 to " ++ (show boardWidth) ++ ") ?")
-- line <- getLine







   
 