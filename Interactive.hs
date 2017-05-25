module Interactive where

import TicTacToe
import GameState

import System.Random

{-
    Jocul între doi utilizatori umani.
-}
twoHumans :: IO ()
twoHumans = interactive console

{-
    Urmărește pas cu pas evoluția jocului, pe baza unei euristici.
-}
humanVsAI :: (Board -> StdGen -> (Cell, Board)) -> IO ()
humanVsAI step = interactive $ \board -> do
    case boardPlayer board of
        X -> console board
        O -> do
            gen <- newStdGen
            let (cell, newBoard) = step board gen
            putStrLn $ "Opponent played cell " ++ show cell
            return $ Just newBoard

{-
    Avansează pas cu pas jocul, pe baza unei strategii primite ca parametru.
    Este folosită atât pentru jocul solitar al utilizatorului,
    cât și pentru urmărirea pas cu pas a jocului bazat pe euristică.
-}
interactive :: (Board -> IO (Maybe Board)) -> IO ()
interactive strategy = loop initialBoard
  where
    loop :: Board -> IO ()
    loop board = do
        print board
        case outcome board of
            Win  _ -> putStrLn $ show (otherPlayer $ boardPlayer board) ++ " wins!"
            Draw _ -> putStrLn "Draw!"
            _      -> do
                mNewBoard <- strategy board
                loop =<< case mNewBoard of
                    Nothing -> do
                        putStrLn "Invalid move!"
                        return board
                    Just newBoard -> return newBoard

{-
    Utilizată atât între doi jucători umani, cât și în jocul utilizatorului
    cu calculatorul, pentru citirea celulei de la consolă.
-}
console :: Board -> IO (Maybe Board)
console board = do
    putStr "Cell (1-9): "
    cell <- getLine
    return $ place (read cell :: Int) board
