import Connect4
import Text.Read

-- WARNING: Side effects beyond this point!

main :: IO ()
main = do
    putStrLn $ "Functional Connect " ++ show connectLength ++ "\nAt any time, enter q to quit or r to restart:"
    play newGameState                                
    
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
