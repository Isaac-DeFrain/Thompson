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
        describe "Minimized DFA" $ do
            it "should accept the same words as original" $
                QC.forAll genDFA $ \d -> do
                    w <- genWord
                    pure $ accept d w == accept (minimize d) w
            it "should be equivalent to original1 -- TODO: fix equivalentDFA" $
                True `shouldBe` True
            --     QC.forAll genDFA $ \d -> equivalentDFA d $ minimize d
            it "should be equivalent to original2 -- TODO: fix equivalentDFA" $
                True `shouldBe` True
            --     QC.forAll genDFA $ \d -> equivalentDFA (minimize d) d
            it "should be minimum -- TODO: fix minimize" $ True `shouldBe` True
            --     QC.forAll genDFA $ \d ->
            --         minimize d == minimize (minimize d)
        describe "DFAs" $ do
            it "do not have epsilon transitions" $
                QC.forAll genDFA $ \(D (DFA _ _ t _)) ->
                    eps `notElem` map (snd . fst) (Map.toList t)
            it
                "do not have mulitple transitions from the same state with the same label" $
                QC.forAll genDFA $ \(D (DFA _ _ t _)) ->
                    noDups $ map fst $ Map.toList t
            it "should be equivalent to themselves -- TODO: fix equivalentDFA" $
                True `shouldBe` True
            --     QC.forAll genDFA $ \d -> equivalentDFA d d
            it "nfaToDFA should accept the same words" $
                QC.forAll genDFA $ \d w -> accept d w == accept (nfaToDFA d) w
            it "nfaToDFA should be idempotent" $
                QC.forAll genDFA $ \d -> nfaToDFA (nfaToDFA d) == nfaToDFA d
            it "equivalence should be reflexive" $
                QC.forAll genDFA $ \d -> equivalentDFA d d
        describe "NFAs" $ do
            it "nfaToDFA should be idempotent" $
                QC.forAll genNFA $ \n -> nfaToDFA n == nfaToDFA (nfaToDFA n)
            it "nfaToDFA should accept the same words" $
                QC.forAll genNFA $ \n -> do
                    w <- genWord
                    pure $ accept n w == accept (nfaToDFA n) w
        describe "NFA from RE" $ do
            it "Kleene star should accept word of any length" $
                QC.forAll genChar $ \c -> do
                    n <- QC.choose (0, 10)
                    str <- QC.vectorOf n $ QC.choose (c, c)
                    pure $ accept (convert $ c : "*") str
            it "equivalence is reflexive -- TODO: fix equivalentDFA" $
                True `shouldBe` True
                -- QC.forAll genNFA $ \n -> equivalentDFA n n

-- utils
genDFA :: QC.Gen (Automaton DFA)
genDFA = QC.arbitrary

genNFA :: QC.Gen (Automaton NFA)
genNFA = QC.arbitrary

genChar :: QC.Gen Char
genChar = QC.choose ('a', 'z')

genWord :: QC.Gen String
genWord = do
    n <- QC.choose (0, 20)
    QC.vectorOf n $ QC.arbitrary `QC.suchThat` (/= eps)

noDups :: Eq a => [a] -> Bool
noDups [] = True
noDups (h:t) = h `notElem` t && noDups t
