{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import qualified Data.Map as Map
import Lib

-- import RegExp
import Test.Hspec
import qualified Test.QuickCheck as QC
import Thompson

-- TODO: use QC.quickCheckWith
symbols :: [Symbol]
symbols = ['a' .. 'z']

arbitraryDFA :: QC.Gen Automaton
arbitraryDFA = QC.arbitrary

genChar :: QC.Gen Char
genChar = QC.choose ('a', 'z')

noDups :: Eq a => [a] -> Bool
noDups [] = True
noDups (h:t) = h `notElem` t && noDups t

main :: IO ()
main =
    hspec $ do
        describe "Minimized DFA" $
            -- | "accept == accept . minimize"
            it "should accept the same words as original" $
            QC.forAll arbitraryDFA $ \d w -> accept d w == accept (minimize d) w
            -- it "should be equivalent to original" $
            --     QC.forAll arbitraryDFA $ \d -> equivalent d $ minimize d
            -- it "should be minimum" $
            --     QC.forAll arbitraryDFA $ \d ->
            --         minimize d == (minimize . minimize) d
        describe "DFAs" $ do
            it "do not have epsilon transitions" $
                QC.forAll arbitraryDFA $ \(DFA _ _ t _) ->
                    eps `notElem` map (snd . fst) (Map.toList t)
            it
                "do not have mulitple transitions from the same state with the same label" $
                QC.forAll arbitraryDFA $ \(DFA _ _ t _) ->
                    noDups $ map fst $ Map.toList t
            -- it "should be equivalent to themselves" $
            --     QC.forAll arbitraryDFA $ \d -> equivalent d d
        describe "NFAs do something else" $
            it "should do what I say it does" $ True `shouldBe` True
        describe "NFA from RE" $ do
            it "Kleene star should accept word of any length" $
                QC.forAll genChar $ \c -> do
                    n <- QC.choose (0, 10)
                    str <- QC.vectorOf n $ QC.choose (c, c)
                    pure $ accept (convert $ c : "*") str
            it "second" $ True `shouldBe` True

-- Property tests
-- nfaToDFA . nfaToDFA == nfaToDFA
-- minimize . minimize == minimize
-- equivalent a a == True
-- equivalent a (minimize a) == True
-- equivalent (minimize a) a == True
-- equivalent a (minimize b) == equivalent a b
-- accept a == accept (minimize a)
-- Generating random NFAs
--------------------------------------
-- Example automata
-- DFA
dfaTr1 :: Ddelta
dfaTr1 =
    Map.fromList
        [ (("a", '0'), "b")
        , (("a", '1'), "c")
        , (("b", '0'), "d")
        , (("c", '0'), "a")
        , (("c", '1'), "b")
        , (("d", '0'), "a")
        , (("d", '1'), "b")
        ]

dfa1 :: Automaton
dfa1 = DFA ["a", "b", "c", "d"] "a" dfaTr1 ["c", "d"]

dfaTr2 :: Ddelta
dfaTr2 =
    Map.fromList
        [ (("a", '0'), "b")
        , (("a", '1'), "c")
        , (("b", '0'), "b")
        , (("b", '1'), "c")
        , (("c", '0'), "b")
        , (("c", '1'), "c")
        ]

dfa2 :: Automaton
dfa2 = DFA ["a", "b", "c"] "a" dfaTr2 ["a", "b", "c"]

dfaTr3 :: Ddelta
dfaTr3 =
    Map.fromList
        [ (("a", '0'), "b")
        , (("a", '1'), "c")
        , (("b", '0'), "b")
        , (("b", '1'), "c")
        , (("c", '0'), "d")
        , (("c", '1'), "c")
        , (("d", '0'), "d")
        , (("d", '1'), "c")
        ]

dfa3 :: Automaton
dfa3 = DFA ["a", "b", "c", "d"] "a" dfaTr3 ["a", "b", "c", "d"]

-- NFA
nfaTr1 :: Ndelta
nfaTr1 =
    Map.fromList
        [ (("a", '0'), ["a", "b"])
        , (("a", '1'), ["b"])
        , (("a", eps), ["c"])
        , (("b", '0'), ["c"])
        , (("b", '1'), ["d"])
        , (("c", '0'), ["d"])
        , (("c", '1'), ["c", "d"])
        ]

nfa1 :: Automaton
nfa1 = NFA ["a", "b", "c", "d"] "a" nfaTr1 ["d"]
