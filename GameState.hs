{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module GameState where

{-
    Clasa instanțiată de tipul stărilor unui joc.
    `s` este tipul stărilor, iar `a`, al acțiunilor.
-}
class GameState s a | s -> a where
    {-
        Întoarce numărul jucătorului care urmează să mute în starea curentă.
        Nu există restricții asupra acestor numere.
    -}
    playerIndex :: s -> Int

    {-
        Întoarce numărul de jucători. Același, indiferent de stare.
    -}
    maxPlayers :: s -> Int

    {-
        Pentru o stare dată, întoarce lista stărilor succesoare, fiecare stare
        aflându-se în pereche cu acțiunea care a condus în ea.
    -}
    successors :: s -> [(a, s)]

    {-
        Întoarce semnificația stării date. V. explicația pentru tipul `Outcome`.
    -}
    outcome :: s -> Outcome

{-
    Tipul semnificațiilor stărilor jocului.
-}
data Outcome
    = Win  Float  -- Unul dintre jucători a câștigat, cu valoarea precizată.
    | Draw Float  -- Jocul s-a încheiat cu remiză, cu valoarea precizată.
    | Ongoing     -- Jocul nu s-a terminat.
    deriving (Eq, Show)