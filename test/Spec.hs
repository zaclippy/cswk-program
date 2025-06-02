import Test.Tasty
  ( TestTree (..),
    defaultMain,
    testGroup, TestName
  )
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import System.Console.ANSI (clearScreen)
import HandParser (handFromStr)
import HandRankings 
import Cards

{-
There are no tests provided by default. If you wish
to write some tests as part of your program, then
you can use the Tasty library---see some of the
lab sheets for how that can be done.
-}

main :: IO ()
main = do
    clearScreen
    defaultMain $ testGroup "Tests"
        [   
            testInvalid,
            testRF,
            testSF,
            test4K,
            testFH,
            testF,
            testS,
            test3K,
            test2P,
            testP,
            testHC
        ]

getRank :: String -> Ranking
getRank str = getHandRanking (handFromStr str)

testInvalid :: TestTree
testInvalid = testGroup "Test for invalid"
    [ 
        testCase "6♠ 2♦ 6♥ 6♣" $
        getRank t1 @?= InvalidHand,
      testCase "6♠ 2♦ 6♥ 6♣ 2♦" $
        getRank t2 @?= InvalidHand 
    ]
    where 
        t1 = "6s 2d 6h 6c" -- not enough
        t2 = "6s 2d 6h 6c 2d" -- duplicate card

testRF :: TestTree
testRF = testGroup "Test Royal Flush"
    [
        testCase "10♥ J♥ Q♥ K♥ A♥" $ 
          getRank "Th Jh Qh Kh Ah" @?= RoyalFlush
    ]

testSF :: TestTree
testSF =
  testGroup
    "Test Straight Flush"
    [ testCase "5◆ 6◆ 3◆ 4◆ 7◆" $
        getRank "5d 6d 3d 4d 7d" @?= StraightFlush,
        testCase "5♥ 3♥ 4♥ 2♥ A♥" $
          getRank "5h 3h 4h 2h Ah" @?= StraightFlush
    ]

test4K :: TestTree
test4K =
  testGroup
    "Test 4 of a kind"
    [ testCase (show $ H (handFromStr "6s 6d 6h 6c Ks")) $
        getRank "6s 6d 6h 6c Ks" @?= FourOfAKind,
      testCase (show $ H (handFromStr "2h 2s 4c 2c 2d")) $
        getRank "2h 2s 4c 2c 2d" @?= FourOfAKind
    ]

testFH :: TestTree
testFH =
  testGroup
    "Test full house"
    [ testCase (show $ H (handFromStr "2h 2d 7d 2s 7s"))$
        getRank "2h 2d 7d 2s 7s" @?= FullHouse,
      testCase (show $ H (handFromStr "Kc Kh Kd 7c 7s")) $
        getRank "Kc Kh Kd 7c 7s" @?= FullHouse
    ]

testF :: TestTree
testF =
  testGroup
    "Test Flush"
    [ testCase (show $ H (handFromStr t1)) $
        getRank t1 @?= Flush
    ]
  where
    t1 = "Kc 9c 2c 8c Qc"

testS :: TestTree
testS =
  testGroup
    "Test Straight"
    [ testCase (show $ H (handFromStr t1)) $
        getRank t1 @?= Straight,
      testCase (show $ H (handFromStr t2)) $
        getRank t2 @?= Straight,
      testCase (show $ H (handFromStr t3)) $
        getRank t3 @?= HighCard 
    ]
  where
    -- Straight:
    t1 = "3c 5d 4h 6c 7s"
    t2 = "Qd Jh Kc Ts 9h"
    -- NOT STRAIGHT:
    t3 = "Jh Qs Kd Ah 2c"

test3K :: TestTree
test3K =
  testGroup
    "Test 3 of a Kind"
    [ testCase (show $ H (handFromStr t1)) $
        getRank t1 @?= ThreeOfAKind
    ]
  where
    t1 = "Kc Kh 7c Kd 5s"

test2P :: TestTree
test2P =
  testGroup
    "Test 2 pair"
    [ testCase (show $ H (handFromStr "Kc Kh 7d 7c 5s")) $
        getRank "Kc Kh 7d 7c 5s" @?= TwoPair
    ]

testP :: TestTree
testP =
  testGroup
    "Test Pair"
    [ testCase (show $ H (handFromStr t1)) $
        getRank t1 @?= Pair
    ]
  where
    t1 = "Kc Kh 7d 2c 5s"

testHC :: TestTree
testHC =
  testGroup
    "Test high card"
    [ testCase (show $ H (handFromStr t1)) $
        getRank t1 @?= HighCard 
    ]
  where
    t1 = "Tc 4h 7d Kc 2s"
