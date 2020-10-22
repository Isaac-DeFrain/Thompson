module Thompson where

import Lib
import RegExp

import qualified Data.Map as Map

convert' :: RE -> Automaton
convert' Empty =
    NFA ["s0", "s1"] "s0" (Map.fromList [(("s0", eps), ["s1"])]) ["s1"]
convert' (Const c) =
    NFA ["s0", "s1"] "s0" (Map.fromList [(("s0", c), ["s1"])]) ["s1"]
convert' (Concat r1 r2) =
    NFA (st1 <> st2') s1 (Map.union t1 $ Map.insert (f1, eps) [s2'] t2') f2'
  where
    NFA st1 s1 t1 [f1] = convert' r1
    adj = length st1
    NFA st2 s2 t2 f2 = convert' r2
    st2' = map adjustState st2
    s2' = adjustState s2
    t2' =
        Map.fromList $
        map (\((_s1, c), _s2) -> ((adjustState _s1, c), map adjustState _s2)) $
        Map.toList t2
    f2' = map adjustState f2
    adjustState :: State -> State
    adjustState (_:num) = "s" <> show ((read num :: Int) + adj)
    adjustState "" = error "Invalid state"
convert' _ = undefined

convert :: String -> Automaton
convert = convert' . parseRE
