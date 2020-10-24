module Thompson
    ( convert
    , convertRE
    , equivalent
    ) where

import Lib
import RegExp

import qualified Data.Map as Map

convertRE :: RE -> Automaton NFA
convertRE Empty =
    N $ NFA ["s0", "s1"] "s0" (Map.fromList [(("s0", eps), ["s1"])]) ["s1"]
convertRE (Const c) =
    N $ NFA ["s0", "s1"] "s0" (Map.fromList [(("s0", c), ["s1"])]) ["s1"]
convertRE (Paren r) = convertRE r
convertRE (Concat r1 r2) = N $ NFA allStates s1 allTrans f2'
  where
    N (NFA st1 s1 t1 f1) = convertRE r1
    N (NFA st2 s2 t2 _) = convertRE r2
    adj = length st1
    st2' = adjustStates adj st2
    s2' = adjustState adj s2
    t2' = adjustTrans adj t2
    f2' = [last st2']
    allStates = st1 <> st2'
    allTrans = Map.union t1 $ Map.insert (head f1, eps) [s2'] t2'
convertRE (Alt r1 r2) = N $ NFA allStates "s0" allTrans final
  where
    N (NFA st1 _ t1 _) = convertRE r1
    N (NFA st2 _ t2 _) = convertRE r2
    adj = length st1 + 1
    final = ["s" <> show (length st1 + length st2 + 1)]
    st1' = adjustStates 1 st1
    t1' = adjustTrans 1 t1
    st2' = adjustStates adj st2
    t2' = adjustTrans adj t2
    allStates = ["s0"] <> st1' <> st2' <> final
    addTrans0 = Map.singleton ("s0", eps) [head st1', head st2']
    addTrans1 = Map.singleton (last st1', eps) final
    addTrans2 = Map.singleton (last st2', eps) final
    allTrans = Map.unions [t1', t2', addTrans0, addTrans1, addTrans2]
convertRE (Star r) = N $ NFA allStates "s0" allTrans final
  where
    N (NFA st _ t _) = convertRE r
    st' = adjustStates 1 st
    t' = adjustTrans 1 t
    final = ["s" <> show (length st + 1)]
    allStates = ["s0"] <> st' <> final
    addTrans0 = Map.singleton ("s0", eps) $ [head st'] <> final
    addTrans1 = Map.singleton (last st', eps) $ [head st'] <> final
    allTrans = Map.unions [t', addTrans0, addTrans1]

-- | convert RE string to an NFA using Thompson's construction
convert :: String -> Automaton NFA
convert = convertRE . parseRE

equivalent :: String -> String -> Bool
equivalent r1 r2 = equivalentDFA (minimize $ convert r1) $ minimize $ convert r2

-- utils
adjustState :: Int -> State -> State
adjustState adj (_:num) = "s" <> show ((read num :: Int) + adj)
adjustState _ "" = error "Invalid state"

adjustStates :: Int -> [State] -> [State]
adjustStates adj = map $ adjustState adj

adjustTrans :: Int -> Ndelta -> Ndelta
adjustTrans adj t =
    Map.fromList $
    map (\((_s1, c), _s2) ->
             ((adjustState adj _s1, c), map (adjustState adj) _s2)) $
    Map.toList t
