{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import qualified Data.Map as Map
import Lib

-- import RegExp
import Test.Hspec
import qualified Test.QuickCheck as QC
import Thompson

-- TODO: use QC.quickCheckWith
main :: IO ()
main =
    hspec $ do
        describe "Minimized DFA" $
            -- | "accept == accept . minimize"
         do
            it "should accept the same words as original" $
                QC.forAll genDFA $ \d w -> accept d w == accept (minimize d) w
            it "should be equivalent to original1 -- TODO" $
                True `shouldBe` True
            --     QC.forAll genDFA $ \d -> equivalent d $ minimize d
            it "should be equivalent to original2 -- TODO" $
                True `shouldBe` True
            --     QC.forAll genDFA $ \d -> equivalent (minimize d) d
            it "should be minimum -- TODO" $ True `shouldBe` True
            --     QC.forAll genDFA $ \d ->
            --         minimize d == minimize (minimize d)
        describe "DFAs" $ do
            it "do not have epsilon transitions" $
                QC.forAll genDFA $ \(DFA _ _ t _) ->
                    eps `notElem` map (snd . fst) (Map.toList t)
            it
                "do not have mulitple transitions from the same state with the same label" $
                QC.forAll genDFA $ \(DFA _ _ t _) ->
                    noDups $ map fst $ Map.toList t
            -- it "should be equivalent to themselves" $
            --     QC.forAll genDFA $ \d -> equivalent d d
            it "nfaToDFA . nfaToDFA == nfaToDFA" $
                QC.forAll genDFA $ \d -> nfaToDFA (nfaToDFA d) == nfaToDFA d
            it "equivalence is reflexive" $
                QC.forAll genDFA $ \d -> equivalent d d
            it
                "should be equivalent to minimized iff equivalent to orinigal -- TODO" $
                True `shouldBe` True
                -- QC.forAll genDFA $ \d1 d2 ->
                --     let m = minimize d2
                --         e = equivalent d1 m
                --         e' = equivalent d1 d2
                --      in e && e' || not e && not e'
        describe "NFAs" $
            it "nfaToDFA is idempotent -- TODO" $ True `shouldBe` True
        describe "NFA from RE" $ do
            it "Kleene star should accept word of any length" $
                QC.forAll genChar $ \c -> do
                    n <- QC.choose (0, 10)
                    str <- QC.vectorOf n $ QC.choose (c, c)
                    pure $ accept (convert $ c : "*") str
            it "equivalence is reflexive -- TODO" $ True `shouldBe` True
                -- QC.forAll genNFA $ \d -> equivalent d d

-- Property tests
-- equivalent a (minimize b) == equivalent a b
-- utils
genDFA :: QC.Gen Automaton
genDFA = QC.arbitrary

-- genNFA :: QC.Gen Automaton
-- genNFA = QC.arbitrary
genChar :: QC.Gen Char
genChar = QC.choose ('a', 'z')

noDups :: Eq a => [a] -> Bool
noDups [] = True
noDups (h:t) = h `notElem` t && noDups t
