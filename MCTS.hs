{-# LANGUAGE MultiParamTypeClasses #-}

module MCTS where

import GameState

import Prelude hiding (traverse)
import System.Random
import Data.Ord
import Data.List

{-
    *** TODO ***

    Implementați tipul `Tree s a`, al arborilor de căutare, unde `s` reprezintă
    tipul stărilor, iar `a`, tipul acțiunilor.

    Sunt necesare câmpuri, precum:
    * starea curentă
    * acțiunea prin care s-a ajuns la stare
    * numărul de vizitări
    * scorul
    * copiii.
-}

type Ni = Int
type Scor = Float

data Tree s a = Empty | Root s Ni Scor [Tree s a] | Node s a Ni Scor [Tree s a]

{-
    *** TODO ***

    Implementați tipul `Zipper s a`, pentru parcurgerea arborilor de căutare
    de tipul `Tree s a`, unde `s` reprezintă tipul stărilor, iar `a`, tipul
    acțiunilor.

    Pe lângă componentele specifice unui zipper (vezi tutorialul din enunț),
    se va reține și un generator de numere aleatoare, modificat pe parcursul
    explorării arborelui.
-}
data Crumb s a = CrumbN s a Ni Scor [Tree s a] [Tree s a] | CrumbR s Ni Scor [Tree s a] [Tree s a]

data Zipper s a = Zipper (Tree s a) [Crumb s a] StdGen

{-
    *** TODO ***

    Instanțiați clasa `Show` cu tipul `Tree s a`.
-}
instance (Show s, Show a) => Show (Tree s a) where
    show (Node s a ni scor children) = show s++" "++show a++" "++(show ni)++" "++(show scor)++"\n"++show children++"\n"
    show (Root s ni scor children) = (show s)++" "++(show ni)++" "++show scor++"\n"++(show children)++"\n"

{-
    ****************
    Funcții de acces
    ****************
-}

{-
    *** TODO ***

    Întoarce starea asociată unui nod.
-}
treeState :: Tree s a -> s
treeState (Node s a ni scor children) = s
treeState (Root s ni scor children) = s

{-
    *** TODO ***

    Întoarce starea asociată unui nod.
-}
treeAction :: Tree s a -> a
treeAction (Node s a ni scor children) = a
treeAction (Root s ni scor children) = undefined

{-
    *** TODO ***

    Întoarce scorul unui nod.
-}
treeScore :: Tree s a -> Float
treeScore (Node s a ni scor children) = scor
treeScore (Root s ni scor children) = scor

{-
    *** TODO ***

    Întoarce numărul de vizitări ale unui nod.
-}
treeVisits :: Tree s a -> Int
treeVisits (Node s a ni scor children) = ni
treeVisits (Root s ni scor children) = ni

{-
    *** TODO ***

    Întoarce copiii unui nod.
-}
treeChildren :: Tree s a -> [Tree s a]
treeChildren (Node s a ni scor children) = children
treeChildren (Root s ni scor children) = children

{-
    *** TODO ***

    Întoarce nodul pe care este centrat zipper-ul.
-}
zipperTree :: Zipper s a -> Tree s a
zipperTree (Zipper node crumbs gen) = node

{-
    *** TODO ***

    Întoarce generatorul de numere aleatoare din interiorul zipper-ului.
-}
zipperGen :: Zipper s a -> StdGen
zipperGen (Zipper node crumbs gen) = gen

{-
    *****************
    Funcții pe arbori
    *****************
-}

{-
    *** TODO ***

    Construiește un arbore de căutare (eventual infinit), pornind de la funcția
    de generare a succesoarelor unei stări și de la starea inițială.
-}
expand :: (s -> [(a, s)])  -- Generatorul stărilor succesoare
       -> s                -- Starea inițială
       -> Tree s a         -- Arborele de căutare

expand succ s0 =  (Root s0 0 0 [getNode succ s a | (a, s)<-(succ s0)])
	where
		getNode succ s a = Node s a 0 0 [getNode succ x y | (y, x)<-(succ s)]

{-
    *** TODO ***

    Explorează arborele, alegând la fiecare pas un succesor aleator,
    până la atingerea unei stări terminale (victorie/ remiză).

    Întoarce:
    * calea urmată, în ordine inversă, astfel că primul element din cale
      este nodul terminal
    * semnificația stării terminale (victorie/ remiză)
    * varianta finală a generatorului de numere aleatoare.
-}
rolloutTree :: GameState s a => Tree s a -> StdGen -> ([Tree s a], Outcome, StdGen)
rolloutTree tree gen = (helper tree gen [])

helper node gen drum
	| (endState state) == 1 = ([node]++drum, state, gen)
	| (endState state) == 0 = (helper (kids!!idx) (snd pair) ([node]++drum))
	where
		state = (outcome (treeState node))
		kids = (treeChildren node)
		pair = next gen
		idx = (fromIntegral (fst pair)) `mod` (length (treeChildren node))

endState Ongoing = 0
endState (Draw x) = 1
endState (Win x) = 1



{-
    *** TODO ***

    Determină cel mai bun copil al unui nod, din perspectiva raportului
    scor / număr de vizitări.

    Hint: `maximumBy` și `comparing`.
-}
bestChild :: Tree s a -> Tree s a
bestChild (Node s a ni scor children) = maximumBy (comparing r) children
	where r (Node s a ni scor children) = scor / (fromIntegral ni)
bestChild (Root s ni scor children) = maximumBy (comparing r) children
	where r (Node s a ni scor children) = scor / (fromIntegral ni)

{-
    *******************
    Funcții de zipper-e
    *******************
-}

{-
    *** TODO ***

    Construiește un zipper centrat pe arborele dat, care stochează generatorul
    de numere aleatoare precizat.
-}
getZipper :: Tree s a -> StdGen -> Zipper s a
getZipper tree gen = (Zipper tree [] gen)

{-
    *** TODO ***

    Verifică dacă zipper-ul este centrat pe rădăcina arborelui.
-}
isRoot :: Zipper s a -> Bool
isRoot (Zipper (Node _ _ _ _ _) _ _) = False
isRoot (Zipper (Root _ _ _ _) _ _) = True

{-
    *** TODO ***

    Valoarea ucb1 din filmuleț (constanta C = 2).
-}
ucb1 :: Float  -- scorul copilului
     -> Int    -- numărul de vizitări ale copilului
     -> Int    -- numărul de vizitări ale părintelui
     -> Float  -- estimarea
ucb1 vi ni n 
	| (ni == 0) = (2/0)
	| otherwise = (vi/fni) + 2 * (sqrt $ (log fn) / fni)
		where 
			fni = (fromIntegral ni)
			fn  = (fromIntegral n)


{-
    *** TODO ***

    Pentru nodul pe care este centrat zipper-ul dat ca parametru, selectează
    copilul având valoarea ucb1 maximă. Întoarce zipper-ul centrat pe copilul
    respectiv.

    Atenție! Așa cum rezultă și din filmuleț, un nod nevizitat are valoarea ucb1
    infinită, și va fi întotdeauna ales în defavoarea altor noduri deja vizitate.
-}
select :: Eq s => Eq a => Zipper s a -> Zipper s a

select (Zipper n@(Node s a ni scor children) crumbs gen) = (Zipper el (bs:crumbs) gen)
	where 
		el = (getMax (head children) (tail children) ni)
		bs = (CrumbN s a ni scor (snd pair) (fst pair))
			where pair = (mySplit el children [])
select (Zipper n@(Root s ni scor children) crumbs gen) = (Zipper el (bs:crumbs) gen)
	where 
		el = (getMax (head children) (tail children) ni)
		bs = (CrumbR s ni scor (snd pair) (fst pair))
			where pair = (mySplit el children [])

getMax max    [] nParent = max
getMax max (n:[]) nParent
	| (ucb1 (treeScore n) (treeVisits n) nParent) > (ucb1 (treeScore max) (treeVisits max) nParent) = n
	| otherwise = max
getMax max (n:list) nParent
	| (ucb1 (treeScore n) (treeVisits n) nParent) > (ucb1 (treeScore max) (treeVisits max) nParent) = (getMax n list nParent)
	| otherwise = (getMax max list nParent)


mySplit :: Eq a => Tree s a -> [Tree s a] -> [Tree s a] -> ([Tree s a], [Tree s a])
mySplit n@(Node s0 a0 n0 scor0 c0) (x@(Node s1 a1 n1 scor1 c1):list1) list2 
	| (a0 /= a1) = (mySplit n list1 (list2++[x]))
	| otherwise = (list1, list2)

{-
    *** TODO ***

    Aplică repetat `select` până la atingerea unui nod nevizitat sau terminal.
-}
traverse :: (Eq s, Eq a, GameState s a) => Zipper s a -> Zipper s a
traverse z@(Zipper n@(Node s a ni scor children) crumbs gen) 
	| ni == 0 = z
	| (endState (outcome s)) == 1 = z
	| otherwise = (traverse (select z))
traverse z@(Zipper n@(Root s ni scor children) crumbs gen) 
	| (endState (outcome s)) == 1 = z
	| otherwise = (traverse (select z))
{-
    *** TODO ***

    Aplică `rolloutTree` pentru arborele pe care este centrat zipper-ul.

    Întoarce:
    * scorul cu care vor fi actualizate nodurile de pe calea către rădăcină
    * numărul jucătorului pentru care se realizează actualizarea
      (se poate ignora pentru cerința cu un singur jucător)
    * noul zipper, actualizat cu generatorul de numere aleatoare întors
      de `rolloutTree`.

    Pentru cerința cu cel puțin doi jucători, scorul și numărul jucătorului
    se calculează astfel:
    * Pentru victorie, se utilizează scorul din obictul `Outcome` și numărul
      jucătorului aferent stării terminale.
    * Pentru remiză, se utilizează scorul din obiectul `Outcome` împărțit
      la numărul de jucători, și `Nothing`.
-}
rolloutZipper :: GameState s a => Zipper s a -> (Float, Maybe Int, Zipper s a)
rolloutZipper z@(Zipper n crumbs gen) = ((getPoints p), player, (Zipper n crumbs gen2))
	where
		p@(trail, result, gen2) = (rolloutTree n gen)
		player = if (getPlayer p) == 1 then Just (playerIndex (treeState (head trail))) else Nothing


getPoints (trail, (Win f), gen) = f
getPoints (trail, (Draw f), gen) = f / (fromIntegral (maxPlayers (treeState (head trail))))
getPlayer (trail, (Win f), gen) = 1
getPlayer (trail, (Draw f), gen) = 0

{-
    *** TODO ***

    Urcă un nivel în arbore.
-}
toParent :: Zipper s a -> Zipper s a
toParent (Zipper n ((CrumbN s a ni scor l1 l2):crumbs) gen) = (Zipper (Node s a ni scor (l1++[n]++l2)) crumbs gen)
toParent (Zipper n ((CrumbR s ni scor l1 l2):crumbs) gen) = (Zipper (Root s ni scor (l1++[n]++l2)) crumbs gen)

{-
    *** TODO ***

    Implementează pasul de backpropagation, unde cei trei parametri sunt cele
    trei componente întoarse de `rolloutZipper`.

    Astfel, se urmează calea către rădăcină și se crește cu 1 numărul
    de vizitări ale tuturor nodurilor. În plus, scorul se modifică astfel:
    * Pentru cerința cu un singur jucător, se modifică toate nodurile.
    * Pentru cerința cu mai mulți jucători, avem următoarele cazuri:
      * În caz de victorie, se actualizează doar nodurile cu numărul de jucător
        dat de parametru.
      * În caz de remiză, se actualizează toate nodurile.
    
    Zipper-ul final este centrat pe rădăcină.
-}
backProp :: GameState s a => Float -> Maybe Int -> Zipper s a -> Zipper s a

backProp scor player z@(Zipper node crumbs gen)
	| (null crumbs) == True = z
	| otherwise = (backProp scor player (goUp scor z player))

goUp toAdd z@(Zipper node ((CrumbN s a ni scor l1 l2):crumbs) gen) player =
	case player of
		Nothing -> (Zipper (Node s a ni scor (l1++[n]++l2)) crumbs gen)
			where 
				n = (Node (treeState node) (treeAction node) ((treeVisits node)+1) ((treeScore node)+toAdd) (treeChildren node))
		Just p -> (Zipper (Node s a ni scor (l1++[n]++l2)) crumbs gen)
			where
				n = (Node (treeState node) (treeAction node) ((treeVisits node)+1) 
					((treeScore node)+toBe) (treeChildren node))
					where
						toBe = (if p == (playerIndex (treeState node)) then toAdd else 0)

goUp final z@(Zipper (Node s0 a0 ni0 scor0 c) ((CrumbR s ni scor l1 l2):[]) gen) player = 
		case player of
			Nothing -> (Zipper (Root s (ni + 1) (scor + final) (l1++[n]++l2)) [] gen) 
				where
					n = (Node s0 a0 (ni0 + 1) (scor0 + final) c)
			Just x -> (Zipper (Root s (ni + 1) (scor + (if (x == (playerIndex s)) then final else 0)) (l1++[n]++l2)) [] gen)
				where
					n = (Node s0 a0 (ni0 + 1) (scor0 + (if (x == (playerIndex s0)) then final else 0)) c)


{-
    *** TODO ***

    Realizează o iterație completă a MCTS, incluzând toate etapele, pornind
    de la un nod oarecare din arbore și finalizând pe rădăcină.
-}
exploreOne :: (Eq s, Eq a, GameState s a) => Zipper s a -> Zipper s a
exploreOne z = backProp a b c 
	where
		(a, b, c) = rolloutZipper $ traverse z

{-
    *** TODO ***

    Realizează un număr dat de iterații complete ale MCTS.
-}
exploreMany :: (Eq s, Eq a, GameState s a) => Int -> Zipper s a -> Zipper s a
exploreMany ct z
	| ct > 0 = exploreMany (ct-1) (exploreOne z)
	| otherwise = z

{-
    *** TODO ***

    Alege o acțiune pornind de la o stare dată și un număr de iterații ale MCTS.
    Întoarce o pereche cu acțiunea și starea următoare.

    Funcția ar trebui să verifice mai întâi dacă nu cumva una dintre stările
    imediat următoare reprezintă o victorie, caz în care o alege direct.
    Altfel, demarează procesul standard de explorare.

    Atenție! La prima iterație a algoritmului, cu toate că numărul de vizitări
    ale rădăcinii este 0, NU se face rollout la rădăcină, ci se trece direct
    la explorarea copiilor acesteia. Acest lucru este vizibil și în filmuleț.

    După realizarea numărului dat de iterații, se alege efectiv acțiunea,
    utilizând `bestChild`.
-}
choose :: (Eq s, Eq a, GameState s a) => Int -> s -> StdGen -> (a, s)
choose ct s gen = (treeAction node, treeState node)
	where node = bestChild $ zipperTree $ exploreMany ct $ Zipper (expand successors s) [] gen
