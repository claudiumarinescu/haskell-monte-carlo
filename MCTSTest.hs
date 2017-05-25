{-# LANGUAGE MultiParamTypeClasses #-}

module MCTSTest where

import MCTS
import TicTacToe
import GameState
import TestPP

import Prelude hiding (traverse)
import Data.List
import Data.Maybe
import Data.Monoid
import System.Random

{-
    Testele utilizează arborele binar de mai jos, cu cheile de la 0 la 14,
    iar sub frunze sunt date outcome-urile acestora, unde W = Win și D = Draw.

                                    0
                    1                               2
            3              4                5               6
        7       8       9       10      11      12      13      14

        W20     W0      D1      D2      W10     W14     D3      D4
-}

-- 1 player

newtype Youtube = Youtube { getYoutube :: Int }
    deriving Eq

instance Show Youtube where
    show (Youtube x) = show x

instance GameState Youtube Int where
    playerIndex = const 1

    maxPlayers = const 1

    successors (Youtube x)
        | x <= 6    = mapM (\f y -> (f y, Youtube $ f y)) actions x
        | otherwise = []
      where
        actions = [(+ 1) . (* 2), (+ 2) . (* 2)]

    outcome (Youtube x) = case x of
        7  -> Win 20
        8  -> Win 0
        9  -> Draw 1
        10 -> Draw 2
        11 -> Win 10
        12 -> Win 14
        13 -> Draw 3
        14 -> Draw 4
        _  -> Ongoing

-- 2 players

newtype Youtube2P = Youtube2P { getYoutube2P :: Int }
    deriving Eq

instance Show Youtube2P where
    show (Youtube2P x) = show x

instance GameState Youtube2P Int where
    -- Nivelul din arbore
    playerIndex (Youtube2P x) = (floor $ logBase 2 $ fromIntegral x + 1) `mod` 2

    maxPlayers = const 2

    successors (Youtube2P x)
        | x <= 6    = mapM (\f y -> (f y, Youtube2P $ f y)) actions x
        | otherwise = []
      where
        actions = [(+ 1) . (* 2), (+ 2) . (* 2)]

    outcome (Youtube2P x) = case x of
        7  -> Win 20
        8  -> Win 0
        9  -> Draw 1
        10 -> Draw 2
        11 -> Win 10
        12 -> Win 14
        13 -> Draw 3
        14 -> Draw 4
        _  -> Ongoing

youtubeTree :: Tree Youtube Int
youtubeTree = expand successors $ Youtube 0

youtubeTreeZipper :: Zipper Youtube Int
youtubeTreeZipper = getZipper youtubeTree $ mkStdGen 0

validScores :: [Float]
validScores = [20, 0, 10, 14, 1, 2, 3, 4]

-- Tests

testTreeShow :: TestPP ()
testTreeShow = testOne 1 $ test "treeShow" 3 "" $ print youtubeTree >> return True

testTreeChildren :: TestPP ()
testTreeChildren = tests 2 1
    [ testVal "treeChildren.youtubeTree.root0"  1 2 $
          length $ treeChildren youtubeTree
    , testVal "treeChildren.youtubeTree.child1" 1 2 $
          length $ treeChildren $ head $ treeChildren youtubeTree
    ]

testTreeState :: TestPP ()
testTreeState = tests 3 1
    [ testVal "treeState.youtubeTree.root0"  1 (Youtube 0) $
          treeState youtubeTree
    , testVal "treeState.youtubeTree.child1" 1 (Youtube 1) $
          treeState $ head $ treeChildren youtubeTree
    ]

testTreeAction :: TestPP ()
testTreeAction = tests 4 1
    [ testVal "treeAction.youtubeTree.child1" 1 1 $
          treeAction $ head $ treeChildren youtubeTree
    ]

testTreeScore :: TestPP ()
testTreeScore = tests 5 1
    [ testVal "treeScore.youtubeTree.root0"  1 0 $ treeScore youtubeTree
    , testVal "treeScore.youtubeTree.child1" 1 0 $
          treeScore $ head $ treeChildren youtubeTree
    ]

testTreeVisits :: TestPP ()
testTreeVisits = tests 6 1
    [ testVal "treeVisits.youtubeTree.root0"  1 0 $ treeVisits youtubeTree
    , testVal "treeVisits.youtubeTree.child1" 1 0 $
          treeVisits $ head $ treeChildren youtubeTree
    ]

testRolloutTree :: TestPP ()
testRolloutTree = tests 7 10
    [ testCond "rolloutTree.youtubeTree.child1.path" 1 $ isChain path1
    , testVal "rolloutTree.youtubeTree.child1.outcome" 1
          (outcome $ treeState $ head path1) $ outcome1
    , testCond "rolloutTree.youtubeTree.child1.terminal" 1 $
          (outcome $ treeState $ head path1) /= Ongoing
    , testCond "rolloutTree.youtubeTree.child1.gen" 1 $ show gen1 /= show gen
    , testCond "rolloutTree.youtubeTree.child2.path" 1 $ isChain path2
    , testVal "rolloutTree.youtubeTree.child2.outcome" 1
          (outcome $ treeState $ head path2) $ outcome2
    , testCond "rolloutTree.youtubeTree.child2.terminal" 1 $
          (outcome $ treeState $ head path2) /= Ongoing
    , testCond "rolloutTree.youtubeTree.child2.gen" 1 $ show gen2 /= show gen
    ]
  where
    gen = mkStdGen 0
    [child1, child2] = treeChildren youtubeTree
    (path1, outcome1, gen1) = rolloutTree child1 gen
    (path2, outcome2, gen2) = rolloutTree child2 gen

    isChain :: Eq s => [Tree s a] -> Bool
    isChain path = and $ zipWith
        (\child parent -> treeState child `elem` map treeState (treeChildren parent))
        path
        (tail path)

testZipperTree :: TestPP ()
testZipperTree = testOne 8 $ testCond "zipperTree" 1 $
    show youtubeTree == show (zipperTree youtubeTreeZipper)

testZipperGen :: TestPP ()
testZipperGen = testOne 9 $ testCond "zipperGen" 1 $
    show (mkStdGen 0) == show (zipperGen youtubeTreeZipper)

testUcb1 :: TestPP ()
testUcb1 = tests 10 5
    [ testCond "ucb1.1" 1 $ ucb1 20 1 2 ~= 21.67
    , testCond "ucb1.2" 1 $ ucb1 10 1 2 ~= 11.67
    , testCond "ucb1.3" 1 $ ucb1 20 2 3 ~= 11.48
    , testCond "ucb1.4" 1 $ ucb1 10 1 3 ~= 12.1
    ]
  where
    (~=) :: Float -> Float -> Bool
    x ~= y = abs (x - y) < 0.01

testSelectIsRootToParent :: TestPP ()
testSelectIsRootToParent = tests 11 5
    [ testCond "testSelectIsRoot.root" 1 $
          isRoot youtubeTreeZipper
    , testCond "testSelectIsRoot.select" 1 $
          not $ isRoot $ select $ youtubeTreeZipper
    , testCond "testSelectIsRoot.toParent" 1 $
          isRoot $ toParent $ select $ youtubeTreeZipper
    ]

-- Pașii din filmuleț
testSelectBackProp :: TestPP ()
testSelectBackProp = tests 12 10
    [ testVal "selectBackProp.child1" 1 (Youtube 1) $
          treeState $ zipperTree child1Zipper
    , testVal "selectBackProp.b20.root0.state" 1 (Youtube 0) $
          treeState $ zipperTree b20Zipper
    , testVal "selectBackProp.b20.root0.score" 1 20 $
          treeScore $ zipperTree b20Zipper
    , testVal "selectBackProp.b20.root0.visits" 1 1 $
          treeVisits $ zipperTree b20Zipper
    , testVal "selectBackProp.b20.child1.score" 1 20 $
          treeScore $ fromJust $ childByState (Youtube 1) $ zipperTree b20Zipper
    , testVal "selectBackProp.b20.child1.visits" 1 1 $
          treeVisits $ fromJust $ childByState (Youtube 1) $ zipperTree b20Zipper
    , testVal "selectBackProp.child2" 1 (Youtube 2) $
          treeState $ zipperTree child2Zipper
    , testVal "selectBackProp.b10.root0.state" 1 (Youtube 0) $
          treeState $ zipperTree b10Zipper
    , testVal "selectBackProp.b10.root0.score" 1 30 $
          treeScore $ zipperTree b10Zipper
    , testVal "selectBackProp.b10.root0.visits" 1 2 $
          treeVisits $ zipperTree b10Zipper
    , testVal "selectBackProp.b10.child2.score" 1 10 $
          treeScore $ fromJust $ childByState (Youtube 2) $ zipperTree b10Zipper
    , testVal "selectBackProp.b10.child2.visits" 1 1 $
          treeVisits $ fromJust $ childByState (Youtube 2) $ zipperTree b10Zipper
    , testVal "selectBackProp.child3" 1 (Youtube 3) $
          treeState $ zipperTree child3Zipper
    , testVal "selectBackProp.b0.root0.state" 1 (Youtube 0) $
          treeState $ zipperTree b0Zipper
    , testVal "selectBackProp.b0.root0.score" 1 30 $
          treeScore $ zipperTree b0Zipper
    , testVal "selectBackProp.b0.root0.visits" 1 3 $
          treeVisits $ zipperTree b0Zipper
    , testVal "selectBackProp.b0.child1.score" 1 20 $
          treeScore $ fromJust $ childByState (Youtube 1) $ zipperTree b0Zipper
    , testVal "selectBackProp.b0.child1.visits" 1 2 $
          treeVisits $ fromJust $ childByState (Youtube 1) $ zipperTree b0Zipper
    , testVal "selectBackProp.b0.child3.score" 1 0 $
          treeScore $ fromJust $ childByState (Youtube 3) $ zipperTree b0Zipper
    , testVal "selectBackProp.b0.child3.visits" 1 1 $
          treeVisits $ fromJust $ childByState (Youtube 3) $ zipperTree b0Zipper
    , testVal "selectBackProp.child5" 1 (Youtube 5) $
          treeState $ zipperTree child5Zipper
    , testVal "selectBackProp.b14.root0.state" 1 (Youtube 0) $
          treeState $ zipperTree b14Zipper
    , testVal "selectBackProp.b14.root0.score" 1 44 $
          treeScore $ zipperTree b14Zipper
    , testVal "selectBackProp.b14.root0.visits" 1 4 $
          treeVisits $ zipperTree b14Zipper
    , testVal "selectBackProp.b14.child2.score" 1 24 $
          treeScore $ fromJust $ childByState (Youtube 2) $ zipperTree b14Zipper
    , testVal "selectBackProp.b14.child2.visits" 1 2 $
          treeVisits $ fromJust $ childByState (Youtube 2) $ zipperTree b14Zipper
    , testVal "selectBackProp.b14.child5.score" 1 14 $
          treeScore $ fromJust $ childByState (Youtube 5) $ zipperTree b14Zipper
    , testVal "selectBackProp.b14.child5.visits" 1 1 $
          treeVisits $ fromJust $ childByState (Youtube 5) $ zipperTree b14Zipper
    ]
  where
    child1Zipper = select youtubeTreeZipper
    b20Zipper = backProp 20 Nothing child1Zipper
    child2Zipper = select b20Zipper
    b10Zipper = backProp 10 Nothing child2Zipper
    child3Zipper = select $ select b10Zipper
    b0Zipper = backProp 0 Nothing child3Zipper
    child5Zipper = select $ select b0Zipper
    b14Zipper = backProp 14 Nothing child5Zipper

-- La fel ca mai sus, dar cu `traverse` în loc de select.
testTraverseBackProp :: TestPP ()
testTraverseBackProp = tests 13 10
    [ testVal "traverseBackProp.child1" 1 (Youtube 1) $
          treeState $ zipperTree child1Zipper
    , testVal "traverseBackProp.b20.root0.state" 1 (Youtube 0) $
          treeState $ zipperTree b20Zipper
    , testVal "traverseBackProp.b20.root0.score" 1 20 $
          treeScore $ zipperTree b20Zipper
    , testVal "traverseBackProp.b20.root0.visits" 1 1 $
          treeVisits $ zipperTree b20Zipper
    , testVal "traverseBackProp.b20.child1.score" 1 20 $
          treeScore $ fromJust $ childByState (Youtube 1) $ zipperTree b20Zipper
    , testVal "traverseBackProp.b20.child1.visits" 1 1 $
          treeVisits $ fromJust $ childByState (Youtube 1) $ zipperTree b20Zipper
    , testVal "traverseBackProp.child2" 1 (Youtube 2) $
          treeState $ zipperTree child2Zipper
    , testVal "traverseBackProp.b10.root0.state" 1 (Youtube 0) $
          treeState $ zipperTree b10Zipper
    , testVal "traverseBackProp.b10.root0.score" 1 30 $
          treeScore $ zipperTree b10Zipper
    , testVal "traverseBackProp.b10.root0.visits" 1 2 $
          treeVisits $ zipperTree b10Zipper
    , testVal "traverseBackProp.b10.child2.score" 1 10 $
          treeScore $ fromJust $ childByState (Youtube 2) $ zipperTree b10Zipper
    , testVal "traverseBackProp.b10.child2.visits" 1 1 $
          treeVisits $ fromJust $ childByState (Youtube 2) $ zipperTree b10Zipper
    , testVal "traverseBackProp.child3" 1 (Youtube 3) $
          treeState $ zipperTree child3Zipper
    , testVal "traverseBackProp.b0.root0.state" 1 (Youtube 0) $
          treeState $ zipperTree b0Zipper
    , testVal "traverseBackProp.b0.root0.score" 1 30 $
          treeScore $ zipperTree b0Zipper
    , testVal "traverseBackProp.b0.root0.visits" 1 3 $
          treeVisits $ zipperTree b0Zipper
    , testVal "traverseBackProp.b0.child1.score" 1 20 $
          treeScore $ fromJust $ childByState (Youtube 1) $ zipperTree b0Zipper
    , testVal "traverseBackProp.b0.child1.visits" 1 2 $
          treeVisits $ fromJust $ childByState (Youtube 1) $ zipperTree b0Zipper
    , testVal "traverseBackProp.b0.child3.score" 1 0 $
          treeScore $ fromJust $ childByState (Youtube 3) $ zipperTree b0Zipper
    , testVal "traverseBackProp.b0.child3.visits" 1 1 $
          treeVisits $ fromJust $ childByState (Youtube 3) $ zipperTree b0Zipper
    , testVal "traverseBackProp.child5" 1 (Youtube 5) $
          treeState $ zipperTree child5Zipper
    , testVal "traverseBackProp.b14.root0.state" 1 (Youtube 0) $
          treeState $ zipperTree b14Zipper
    , testVal "traverseBackProp.b14.root0.score" 1 44 $
          treeScore $ zipperTree b14Zipper
    , testVal "traverseBackProp.b14.root0.visits" 1 4 $
          treeVisits $ zipperTree b14Zipper
    , testVal "traverseBackProp.b14.child2.score" 1 24 $
          treeScore $ fromJust $ childByState (Youtube 2) $ zipperTree b14Zipper
    , testVal "traverseBackProp.b14.child2.visits" 1 2 $
          treeVisits $ fromJust $ childByState (Youtube 2) $ zipperTree b14Zipper
    , testVal "traverseBackProp.b14.child5.score" 1 14 $
          treeScore $ fromJust $ childByState (Youtube 5) $ zipperTree b14Zipper
    , testVal "traverseBackProp.b14.child5.visits" 1 1 $
          treeVisits $ fromJust $ childByState (Youtube 5) $ zipperTree b14Zipper
    ]
  where
    child1Zipper = select youtubeTreeZipper
    b20Zipper = backProp 20 Nothing child1Zipper
    child2Zipper = traverse b20Zipper
    b10Zipper = backProp 10 Nothing child2Zipper
    child3Zipper = traverse b10Zipper
    b0Zipper = backProp 0 Nothing child3Zipper
    child5Zipper = traverse b0Zipper
    b14Zipper = backProp 14 Nothing child5Zipper

testRolloutZipper :: TestPP ()
testRolloutZipper = tests 14 10
    [ testCond "rolloutZipper.1.score.valid" 1 $
          score1 `elem` validScores
    , testCond "rolloutZipper.1.gen.different" 1 $
          show (zipperGen zipper1) /= show (zipperGen youtubeTreeZipper)
    , testCond "rolloutZipper.2.score.valid" 1 $
          score2 `elem` validScores
    , testCond "rolloutZipper.2.score.different" 1 $
          score2 /= score1
    , testCond "rolloutZipper.2.gen.different" 1 $
          show (zipperGen zipper2) /= show (zipperGen zipper1)
    ]
  where
    (score1, _, zipper1) = rolloutZipper youtubeTreeZipper
    (score2, _, zipper2) = rolloutZipper zipper1

testExploreOne :: TestPP ()
testExploreOne = tests 15 5
    [ testVal "exploreOne.1.root0.state" 1 (Youtube 0) $
          treeState $ zipperTree zipper1
    , testCond "exploreOne.1.root0.score" 1 $
          treeScore (zipperTree zipper1) `elem` validScores
    , testVal "exploreOne.1.root0.visits" 1 1 $
          treeVisits (zipperTree zipper1)
    , testCond "exploreOne.1.gen.different" 1 $
          show (zipperGen zipper1) /= show (zipperGen youtubeTreeZipper)
    , testVal "exploreOne.2.root0.state" 1 (Youtube 0) $
          treeState $ zipperTree zipper2
    , testCond "exploreOne.2.root0.score" 1 $
          treeScore (zipperTree zipper2) `elem`
              [ x + y | x <- validScores, y <- validScores ]
    , testVal "exploreOne.2.root0.visits" 1 2 $
          treeVisits (zipperTree zipper2)
    , testCond "exploreOne.2.gen.different" 1 $
          show (zipperGen zipper2) /= show (zipperGen zipper1)
    ]
  where
    zipper1 = exploreOne $ select youtubeTreeZipper
    zipper2 = exploreOne zipper1

testExploreMany :: TestPP ()
testExploreMany = tests 16 5
    [ testVal "exploreMany.2.root0.state" 1 (Youtube 0) $
          treeState $ zipperTree zipper2
    , testCond "exploreMany.2.root0.score" 1 $
          treeScore (zipperTree zipper2) `elem`
              (map sum $ sequence $ replicate 2 validScores)
    , testVal "exploreMany.2.root0.visits" 1 2 $
          treeVisits (zipperTree zipper2)
    , testCond "exploreMany.2.gen.different" 1 $
          show (zipperGen zipper2) /= show (zipperGen youtubeTreeZipper)
    , testVal "exploreMany.3.root0.state" 1 (Youtube 0) $
          treeState $ zipperTree zipper3
    , testCond "exploreMany.3.root0.score" 1 $
          treeScore (zipperTree zipper3) `elem`
              (map sum $ sequence $ replicate 3 validScores)
    , testVal "exploreMany.3.root0.visits" 1 3 $
          treeVisits (zipperTree zipper3)
    , testCond "exploreMany.3.gen.different" 1 $
          show (zipperGen zipper3) /= show (zipperGen zipper2)
    ]
  where
    zipper2 = exploreMany 2 $ select $ youtubeTreeZipper
    zipper3 = exploreMany 3 $ select $ youtubeTreeZipper

testBestChild :: TestPP ()
testBestChild = tests 17 5
    [ testVal "bestChild.0" 1 (Youtube 2) $
          treeState $ bestChild $ zipperTree $ exploreMany 100 youtubeTreeZipper
    ]

testChoose :: TestPP ()
testChoose = tests 18 5
    [ testVal "choose.0" 1 (2, Youtube 2) $ choose 100 (Youtube 0) $ mkStdGen 0
    , testVal "choose.1" 1 (3, Youtube 3) $ choose 100 (Youtube 1) $ mkStdGen 0
    , testVal "choose.2" 1 (5, Youtube 5) $ choose 100 (Youtube 2) $ mkStdGen 0
    ]

-- 2 jucători

testTraverseBackProp2P :: TestPP ()
testTraverseBackProp2P = tests 19 10
    [ testVal "traverseBackProp2P.child1" 1 (Youtube2P 1) $
          treeState $ zipperTree child1Zipper
    , testVal "traverseBackProp2P.b20.root0.state" 1 (Youtube2P 0) $
          treeState $ zipperTree b20Zipper
    , testVal "traverseBackProp2P.b20.root0.score" 1 0 $
          treeScore $ zipperTree b20Zipper
    , testVal "traverseBackProp2P.b20.root0.visits" 1 1 $
          treeVisits $ zipperTree b20Zipper
    , testVal "traverseBackProp2P.b20.child1.score" 1 20 $
          treeScore $ fromJust $ childByState (Youtube2P 1) $ zipperTree b20Zipper
    , testVal "traverseBackProp2P.b20.child1.visits" 1 1 $
          treeVisits $ fromJust $ childByState (Youtube2P 1) $ zipperTree b20Zipper
    , testVal "traverseBackProp2P.child2" 1 (Youtube2P 2) $
          treeState $ zipperTree child2Zipper
    , testVal "traverseBackProp2P.b10.root0.state" 1 (Youtube2P 0) $
          treeState $ zipperTree b10Zipper
    , testVal "traverseBackProp2P.b10.root0.score" 1 0 $
          treeScore $ zipperTree b10Zipper
    , testVal "traverseBackProp2P.b10.root0.visits" 1 2 $
          treeVisits $ zipperTree b10Zipper
    , testVal "traverseBackProp2P.b10.child2.score" 1 10 $
          treeScore $ fromJust $ childByState (Youtube2P 2) $ zipperTree b10Zipper
    , testVal "traverseBackProp2P.b10.child2.visits" 1 1 $
          treeVisits $ fromJust $ childByState (Youtube2P 2) $ zipperTree b10Zipper
    , testVal "traverseBackProp2P.child3" 1 (Youtube2P 3) $
          treeState $ zipperTree child3Zipper
    , testVal "traverseBackProp2P.b0.root0.state" 1 (Youtube2P 0) $
          treeState $ zipperTree b0Zipper
    , testVal "traverseBackProp2P.b0.root0.score" 1 0 $
          treeScore $ zipperTree b0Zipper
    , testVal "traverseBackProp2P.b0.root0.visits" 1 3 $
          treeVisits $ zipperTree b0Zipper
    , testVal "traverseBackProp2P.b0.child1.score" 1 20 $
          treeScore $ fromJust $ childByState (Youtube2P 1) $ zipperTree b0Zipper
    , testVal "traverseBackProp2P.b0.child1.visits" 1 2 $
          treeVisits $ fromJust $ childByState (Youtube2P 1) $ zipperTree b0Zipper
    , testVal "traverseBackProp2P.b0.child3.score" 1 0 $
          treeScore $ fromJust $ childByState (Youtube2P 3) $ zipperTree b0Zipper
    , testVal "traverseBackProp2P.b0.child3.visits" 1 1 $
          treeVisits $ fromJust $ childByState (Youtube2P 3) $ zipperTree b0Zipper
    , testVal "traverseBackProp2P.child5" 1 (Youtube2P 5) $
          treeState $ zipperTree child5Zipper
    , testVal "traverseBackProp2P.b14.root0.state" 1 (Youtube2P 0) $
          treeState $ zipperTree b14Zipper
    , testVal "traverseBackProp2P.b14.root0.score" 1 14 $
          treeScore $ zipperTree b14Zipper
    , testVal "traverseBackProp2P.b14.root0.visits" 1 4 $
          treeVisits $ zipperTree b14Zipper
    , testVal "traverseBackProp2P.b14.child2.score" 1 10 $
          treeScore $ fromJust $ childByState (Youtube2P 2) $ zipperTree b14Zipper
    , testVal "traverseBackProp2P.b14.child2.visits" 1 2 $
          treeVisits $ fromJust $ childByState (Youtube2P 2) $ zipperTree b14Zipper
    , testVal "traverseBackProp2P.b14.child5.score" 1 14 $
          treeScore $ fromJust $ childByState (Youtube2P 5) $ zipperTree b14Zipper
    , testVal "traverseBackProp2P.b14.child5.visits" 1 1 $
          treeVisits $ fromJust $ childByState (Youtube2P 5) $ zipperTree b14Zipper
    ]
  where
    child1Zipper = select $
        getZipper (expand successors $ Youtube2P 0) $ mkStdGen 0
    b20Zipper = backProp 20 (Just 1) child1Zipper
    child2Zipper = traverse b20Zipper
    b10Zipper = backProp 10 (Just 1) child2Zipper
    child3Zipper = traverse b10Zipper
    b0Zipper = backProp 0 (Just 0) child3Zipper
    child5Zipper = traverse b0Zipper
    b14Zipper = backProp 14 (Just 0) child5Zipper

testRolloutZipper2P :: TestPP ()
testRolloutZipper2P = tests 20 10
    [ testCond "rolloutZipper2P.child3.score" 1 $ score1 `elem` [20, 0]
    , testVal "rolloutZipper2P.child3.player" 1 (Just 1) player1
    , testCond "rolloutZipper2P.child4.score" 1 $ score2 `elem` [0.5, 1]
    , testVal "rolloutZipper2P.child4.player" 1 Nothing player2
    ]
  where
    (score1, player1, _) = rolloutZipper $
        getZipper (expand successors $ Youtube2P 3) $ mkStdGen 0
    (score2, player2, _) = rolloutZipper $
        getZipper (expand successors $ Youtube2P 4) $ mkStdGen 0

-- Determină urmașul cu starea dată, pe orice nivel sub rădăcina precizată.
childByState :: Eq s => s -> Tree s a -> Maybe (Tree s a)
childByState state tree = getFirst $ mconcat $ map First $
    find ((== state) . treeState) children : map (childByState state) children
  where
    children = treeChildren tree

-- Bonus

testBoardShow :: TestPP ()
testBoardShow = testOne 21 $ test "boardShow" 3 "" $
    print initialBoard >> return True

testBoardConfiguration :: TestPP ()
testBoardConfiguration = tests 22 1 $
    [ testCond "boardConfiguration.initialBoard.Nothing" 1 $
          all (== Nothing) configuration
    , testCond "boardConfiguration.initialBoard.length" 1 $
          length configuration == 9
    ]
  where
    configuration = boardConfiguration initialBoard

testBoardPlayer :: TestPP ()
testBoardPlayer = tests 23 1
    [ testVal "boardPlayer.initialBoard.X" 1 X $ boardPlayer initialBoard
    ]

testPlace :: TestPP ()
testPlace = tests 24 3
    [ testVal "place.X0.Nothing" 1 Nothing $ place 0 x1
    , testVal "place.X1.config" 1 (Just X : replicate 8 Nothing) $
          boardConfiguration x1
    , testVal "place.X1.player" 1 O $ boardPlayer x1
    , testVal "place.X1O1.Nothing" 1 Nothing $ place 1 x1
    , testVal "place.X1O2.config" 1 (Just X : Just O : replicate 7 Nothing) $
          boardConfiguration x1o2
    , testVal "place.X1O2.player" 1 X $ boardPlayer x1o2
    ]
  where
    x1 = fromJust $ place 1 initialBoard
    x1o2 = fromJust $ place 2 x1

testPlayerIndex :: TestPP ()
testPlayerIndex = tests 25 1
    [ testCond "playerIndex.different" 1 $ xIndex /= oIndex
    , testCond "playerIndex.same.X" 1 $ xIndex == playerIndex x1o2
    , testCond "playerIndex.same.O" 1 $ oIndex == playerIndex x1o2x3
    ]
  where
    xIndex = playerIndex initialBoard
    x1 = fromJust $ place 1 initialBoard
    oIndex = playerIndex x1
    x1o2 = fromJust $ place 2 x1
    x1o2x3 = fromJust $ place 3 x1o2

testMaxPlayers :: TestPP ()
testMaxPlayers = tests 25 1
    [ testVal "maxPlayers.initialBoard" 1 2 $ maxPlayers initialBoard
    , testVal "maxPlayers.X1" 1 2 $ maxPlayers x1
    ]
  where
    x1 = fromJust $ place 1 initialBoard

testSuccessors :: TestPP ()
testSuccessors = tests 25 4
    [ testCond "successors.initialBoard" 1 $
          length (successors initialBoard `intersect` succsInitialBoard) == 9
    ]
  where
    succsInitialBoard = zip [1 .. 9] $
                            map (fromJust . flip place initialBoard) [1 .. 9]

testOutcome :: TestPP ()
testOutcome = tests 26 5
    [ testVal "outcome.initialBoard" 1 Ongoing $ outcome initialBoard
    , testVal "outcome.x1o4x2o5" 1 Ongoing $ outcome x1o4x2o5
    , testCond "outcome.x1o4x2o5.win.X" 1 $
          isWin $ outcome $ fromJust $ place 3 x1o4x2o5
    , testCond "outcome.x1o4x2o5.win.O" 1 $
          isWin $ outcome $ fromJust $ placeMany [9, 6] x1o4x2o5
    , testCond "outcome.x1o4x2o5.draw" 1 $
          isDraw $ outcome $ fromJust $ placeMany [6, 3, 7, 8, 9] x1o4x2o5
    ]
  where
    isWin (Win _) = True
    isWin _       = False

    isDraw (Draw _) = True
    isDraw _        = False

    x1o4x2o5 = fromJust $ placeMany [1, 4, 2, 5] initialBoard

testStep :: TestPP ()
testStep = tests 27 1
    [ testVal "step.x1o4x2o5.win.X" 1 (3, fromJust $ place 3 x1o4x2o5) $
          step x1o4x2o5 $ mkStdGen 0
    , testVal "step.x1o4x2o5.block.O" 1 (3, fromJust $ place 3 x1o4x2) $
          step x1o4x2 $ mkStdGen 0
    ]
  where
    x1o4x2o5 = fromJust $ placeMany [1, 4, 2, 5] initialBoard
    x1o4x2 = fromJust $ placeMany [1, 4, 2] initialBoard

-- Realizează mutările în ordinea din listă.
placeMany :: [Cell] -> Board -> Maybe Board
placeMany cells board = foldl (\b c -> b >>= place c) (Just board) cells

main :: IO ()
main = runTestPP $ sequence_ [ testTreeShow
                             , testTreeChildren
                             , testTreeState
                             , testTreeAction
                             , testTreeScore
                             , testTreeVisits
                             , testRolloutTree
                             , testZipperTree
                             , testZipperGen
                             , testUcb1
                             , testSelectIsRootToParent
                             , testSelectBackProp
                             , testTraverseBackProp
                             , testRolloutZipper
                             , testExploreOne
                             , testExploreMany
                             , testBestChild
                             , testChoose
                             -- 2 jucători
                             , testTraverseBackProp2P
                             , testRolloutZipper2P
                             -- bonus
                             , testBoardShow
                             , testBoardConfiguration
                             , testBoardPlayer
                             , testPlace
                             , testPlayerIndex
                             , testMaxPlayers
                             , testSuccessors
                             , testOutcome
                             , testStep
                             ]