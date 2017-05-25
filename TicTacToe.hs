{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, TupleSections #-}

module TicTacToe where

import MCTS
import GameState

import System.Random
import Data.Maybe

{-
    Tipul celulelor (1-9)
-}
type Cell = Int

{-
    Tipul jucătorilor
-}
data Player = X | O
    deriving (Eq, Enum, Show)

{-
    Întoarce celălalt jucător.
-}
otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

{-
    *** TODO ***

    Tipul stării jocului. Ar trebui să conțină informații despre tablă
    și despre jucătorul care urmează să mute.
-}
data Board = Board Player [(Cell, Maybe Player)]
    deriving Eq

{-
    *** TODO ***

    Întoarce lista conținuturilor celulelor, unde celule libere
    sunt reprezentate de `Nothing`.

    Ordinea celulelor este următoarea:

    789
    456
    123
-}
boardConfiguration :: Board -> [Maybe Player]
boardConfiguration (Board p list) = map snd list


{-
    *** TODO ***

    Întoarce jucătorul care urmează să mute.
-}
boardPlayer :: Board -> Player
boardPlayer (Board p list) = p

{-
    *** TODO ***

    Instanțiați clasa `Show` cu tipul `Board`.
-}
instance Show Board where
    show (Board p list) = show p ++ (prettyShow (reverse list))
        where
            prettyShow (x:[]) = (if ((fst x) `mod` 3) == 0 then "\n" else "  ") ++ show x ++ "\n"
            prettyShow (x:list) = (if ((fst x) `mod` 3) == 0 then "\n" else "  ") ++ show x ++ (prettyShow list)



{-
    *** TODO ***

    Instanțiați clasa `GameBoard` cu tipurile `Board` și `Cell`.
-}
instance GameState Board Cell where
    -- playerIndex :: Board -> Int
    playerIndex (Board p list) = case p of
        X -> 0
        O -> 1

    -- maxPlayers :: Board -> Int
    maxPlayers b = 2

    -- successors :: Board -> [(Cell, Board)]
    successors b@(Board player list) = map (\(c,p) -> (c, fromJust (place c b))) succs
        where
            succs = filter (\pair -> (snd pair) == Nothing) list

    -- outcome :: Board -> Outcome
    outcome b@(Board player list) = 
                if (    (check3 (snd (list!!0)) (snd (list!!1)) (snd (list!!2))) 
                    || (check3 (snd (list!!3)) (snd (list!!4)) (snd (list!!5)))
                    || (check3 (snd (list!!6)) (snd (list!!7)) (snd (list!!8)))
                    || (check3 (snd (list!!0)) (snd (list!!3)) (snd (list!!6)))
                    || (check3 (snd (list!!1)) (snd (list!!4)) (snd (list!!7)))
                    || (check3 (snd (list!!2)) (snd (list!!5)) (snd (list!!8)))
                    || (check3 (snd (list!!0)) (snd (list!!4)) (snd (list!!8)))
                    || (check3 (snd (list!!2)) (snd (list!!4)) (snd (list!!6))) ) 
                    then (Win 10)
                    else if (null (successors b)) then (Draw 5)
                        else Ongoing
        where
            check3 a b c = if ((a == b) && (a == c) && (a /= Nothing)) then True else False
                            


{-
    *** TODO ***

    Tabla inițială de joc. X mută primul.
-}
initialBoard :: Board
initialBoard = Board X [(c, Nothing) | c<-[1..9]]

{-
    *** TODO ***

    Mută în celula dată ca parametru, în funcție de jucătorul aflat la rând,
    și schimbă jucătorul curent.

    Ordinea celulelor este explicată la funcția `boardConfiguration`.
-}
place :: Cell -> Board -> Maybe Board
place cell (Board p list) = (if (null cellSearch) == True then Nothing else Just updatedBoard)
    where
        cellSearch = filter (\pair -> ((fst pair) == cell) && ((snd pair) == Nothing)) list
        updatedBoard = (Board (otherPlayer p) (map myFilter list))
            where
                myFilter pair = if (fst pair) == cell then (cell, Just p) else pair




{-
    *** TODO ***

    Alege o mutare pornind din starea curentă.

    Utilizați `choose` din modulul `MCTS`, cu un număr dorit de iterații
    ale algoritmului.

    Pentru a juca contra calculatorului, rulați din modulul `Interactive`:

    > humanVsAI step
-}
step :: Board -> StdGen -> (Cell, Board)
step b@(Board player list) gen = choose 500 b gen
