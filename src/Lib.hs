{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

-- TODO: equivalent Map.! error
-- TODO: Automaton GADT
module Lib
    ( Automaton(..)
    , Ddelta
    , Ndelta
    , State
    , Symbol
    , accept
    , eps
    , equivalent
    , nfaToDFA
    , minimize
    ) where

import qualified Data.Bifunctor as Bi
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import qualified Test.QuickCheck as QC

type Symbol = Char

type State = String

-- | deterministic transition function
type Ddelta = Map.Map (State, Symbol) State

-- | nondeterministic transition function
type Ndelta = Map.Map (State, Symbol) [State]

data Automaton
    = NFA [State] -- ^ states
          State -- ^ start state
          Ndelta -- ^ nondeterministic transition
          [State] -- ^ final states
    | DFA [State] -- ^ states
          State -- ^ start state
          Ddelta -- ^ deterministic transition
          [State] -- ^ final states
    deriving (Eq, Ord)

-- | empty symbol
eps :: Symbol
eps = '_'

-- | arbitrary number of eps transitions before and after any symbol transition
propogate :: Automaton -> Symbol -> State -> [State]
propogate (NFA _ _ t _) sym s =
    let tmp =
            concatMap (\x -> fromMaybe [] (Map.lookup (x, sym) t)) $
            epsClosure t [s]
     in tmp <> epsClosure t tmp
propogate (DFA _ _ t _) sym s = maybe [] pure $ Map.lookup (s, sym) t

-- | epsilon closure of a list of states
epsClosure :: Ndelta -> [State] -> [State]
epsClosure t sts =
    let tmp = concatMap (\st -> st : fromMaybe [] (Map.lookup (st, eps) t)) sts
     in if all (`elem` sts) tmp
            then sts
            else epsClosure t tmp

-- | list of states reached from beginning list of states after consuming all provided symbols
endingStates :: [State] -> Automaton -> [Symbol] -> [State] -> [State]
endingStates _ _ [] st = uniqueSort st
endingStates begin a (x:xs) _ =
    let nextStates = concatMap (propogate a x) begin
     in endingStates nextStates a xs nextStates

-- | sort and delete duplicates
uniqueSort :: (Eq a, Ord a) => [a] -> [a]
uniqueSort l = dedup l []
  where
    dedup [] l' = List.sort l'
    dedup (x:xs) l' = dedup (filter (/= x) xs) $ x : l'

-- | checks whether the automaton accepts the given word (or empty seq)
accept :: Automaton -> [Symbol] -> Bool
accept a [] = any (`elem` endingStates [start a] a [eps] []) $ final a
accept a syms = any (`elem` endingStates [start a] a syms []) $ final a

-- | NFAtoDFA
-- | the intermediate map connects the DFA to its NFA origins
type IntermediateMap = Map.Map State [State]

-- | change the role of keys and values in an invertible map
kvswap :: Ord b => Map.Map a b -> Map.Map b a
kvswap m = Map.fromList $ map (\(a, b) -> (b, a)) $ Map.toList m

-- | general form of renaming states
kvTransMap ::
       (Ord a, Ord b, Ord c) => (a -> b) -> Map.Map (a, c) a -> Map.Map (b, c) b
kvTransMap f m' = Map.mapKeys (Bi.first f) $ Map.map f m'

-- | initialize a map which ultimately creates the DFA transition from an NFA
initializeDFAmap :: Automaton -> IntermediateMap
initializeDFAmap (NFA _ s t _) = Map.singleton "s0" $ epsClosure t [s]
initializeDFAmap _ = error "initializeDFAmap can only be called on an NFA" -- only use NFA as input

-- | given the NFA transition and intermediate map, compute the list of reachable NFA states from the given state and symbol
delta :: Ndelta -> IntermediateMap -> State -> Symbol -> [State]
delta nt im st sym =
    uniqueSort $
    epsClosure nt $
    concatMap (\x -> fromMaybe [] (Map.lookup (x, sym) nt)) $
    fromMaybe [] $ Map.lookup st im

-- | if a genuinely new state is produced, add it to the intermediate map
updateStates :: Ndelta -> IntermediateMap -> State -> Symbol -> IntermediateMap
updateStates nt im st sym =
    let d = delta nt im st sym
     in if d `elem` Map.elems im <> [[]]
            then im
            else Map.insert ("s" <> show (Map.size im)) d im

-- | produces the final intermediate map for the DFA from initializeDFAmap
buildStates :: Ndelta -> IntermediateMap -> [Symbol] -> IntermediateMap
buildStates nt im syms =
    let updated =
            foldr
                (\s m' -> foldr (\sym m -> updateStates nt m s sym) m' syms)
                im $
            Map.keys im
     in if updated == im
            then im
            else buildStates nt updated syms

-- | produces DFA transition from final intermediate map
transitionDFA :: Automaton -> IntermediateMap -> [Symbol] -> Ddelta
transitionDFA (NFA _ _ nt _) im syms =
    foldr
        (\st m' ->
             foldr
                 (\sy m ->
                      case Map.lookup (delta nt im st sy) (kvswap im) of
                          Nothing -> m
                          Just s -> Map.insert (st, sy) s m)
                 m'
                 syms)
        Map.empty $
    Map.keys im
transitionDFA _ _ _ = error "transitionDFA can only be called on an NFA"

-- | final states of the generated DFA
finalStatesDFA :: Automaton -> IntermediateMap -> [State]
finalStatesDFA nfa im =
    map (kvswap im Map.!) $ filter (any (`elem` final nfa)) $ Map.elems im

-- | produces a DFA (unminimized) from an NFA when given an NFA
nfaToDFA :: Automaton -> Automaton
-- TODO: rename states first
nfaToDFA nfa@(NFA _ _ nt _) =
    let im' = initializeDFAmap nfa
        syms = symbols nfa
        im = buildStates nt im' syms
        dt = transitionDFA nfa im syms
        f = finalStatesDFA nfa im
     in DFA (Map.keys im) "s0" dt f
-- rename states whn given a DFA
nfaToDFA (DFA s i dt f) =
    let tmp = map (\x -> "s" <> show x) indices
        m = Map.fromList $ zip s tmp
        s' = map (m Map.!) s
        i' = m Map.! i
        dt' = kvTransMap (m Map.!) dt
        f' = map (m Map.!) f
     in DFA s' i' dt' f'
  where
    indices :: [Int]
    indices = [0 ..]

------------------------
-- Minimizing the DFA --
------------------------
-- | state partition for minimizing a DFA 
type Partition = [[State]]

-- | initial partition == [nonfinal states, final states]
initialPartition :: Automaton -> Partition
initialPartition a = [states a List.\\ f, f]
  where
    f = final a

-- | checks if two states are indistinguishable given a partition and symbol
indistinguishableSym :: Ddelta -> Partition -> State -> State -> Symbol -> Bool
indistinguishableSym dt p s1 s2 sy =
    let c1 = containingSet dt p sy s1
        c2 = containingSet dt p sy s2
     in List.null c1 || List.null c2 || c1 == c2

-- | collections of states within the given partition, contiaining the given state
containingSet :: Ddelta -> Partition -> Symbol -> State -> [State]
containingSet dt p sy st =
    case Map.lookup (st, sy) dt of
        Nothing -> []
        Just s -> fromMaybe [] $ List.find (elem s) p

-- | checks that two states in the same section are indistinguishable in the given a partition
indistinguishable :: Ddelta -> Partition -> State -> State -> Bool
indistinguishable dt p s1 s2 =
    all (indistinguishableSym dt p s1 s2 . snd . fst) $ Map.toList dt

-- | refine partition
refine :: Ddelta -> Partition -> Partition
refine dt p =
    let r = List.sort $ concatMap (refineAcc []) p
     in if r == p
            then p
            else refine dt r
  where
    refineAcc l [s] = [s] : l
    refineAcc l (s:rest) =
        let l' = filter (indistinguishable dt p s) rest
         in if List.null l'
                then refineAcc ([s] : l) rest
                else refineAcc ((s : l') : l) $ rest List.\\ l'
    refineAcc l [] = l

-- | corespondence between original and minimized states
originalToMinStatesMap :: Partition -> Map.Map [State] State
originalToMinStatesMap p =
    foldr
        (\x m -> Map.insert x ("s" <> show (length p - Map.size m - 1)) m)
        Map.empty
        p

-- build transitions for minimized DFA from original transition and given partition
minDelta :: Automaton -> Partition -> Ddelta
minDelta dfa@(DFA _ _ dt _) p =
    let tmp = originalToMinStatesMap p
        tmp' = kvswap tmp
     in foldr
            (\st m' ->
                 foldr
                     (\sy m ->
                          let cs = containingSet dt p sy (head (tmp' Map.! st))
                              t =
                                  tmp Map.!
                                  containingSet dt p sy (head (tmp' Map.! st))
                           in if List.null cs
                                  then m
                                  else Map.insert (st, sy) t m)
                     m'
                     (symbols dfa))
            Map.empty $
        Map.keys tmp'
minDelta _ _ = error "minDelta can only be called on a DFA"

-- | given original DFA and partition, compute minimized final states
minFinalStates :: Automaton -> Partition -> [State]
minFinalStates (DFA _ _ _ f) p =
    uniqueSort $ map ((originalToMinStatesMap p Map.!) . firstWithin p) f
  where
    firstWithin (h:t) x =
        if x `elem` h
            then h
            else firstWithin t x
    firstWithin _ _ = error "firstWithin can only be called on a nonempty list"
minFinalStates _ _ = error "minFinalStates can only be called on a DFA"

-- | connects original start state to minimized start state
minInitialState :: Automaton -> Partition -> State
minInitialState (DFA _ i _ _) p = head $ first (elem i) p
  where
    first pr (h:t) =
        if pr h
            then h
            else first pr t
    first _ _ = error "first can only be called on a nonempty list"
minInitialState _ _ = error "minInitialStates can only be called on a DFA"

-- | minimize DFA
minimize :: Automaton -> Automaton
minimize dfa@DFA {} =
    let d = nfaToDFA dfa
        DFA _ _ t _ = d
        p = refine t $ initialPartition d
        mt = minDelta d p
        i = minInitialState d p
        fs = minFinalStates d p
        st = uniqueSort $ map fst $ Map.keys mt
     in DFA st i mt fs
minimize nfa = minimize $ nfaToDFA nfa

-- | equivalence of automata
-- | relabel states & minimize
-- | check number of states & final states
-- | permute state labels & check for equality
equivalent :: Automaton -> Automaton -> Bool
equivalent a1 a2 =
    a1 == a2 ||
    let ma1 = minimize $ nfaToDFA a1
        ma2 = minimize $ nfaToDFA a2
        sa1 = states ma1
     in sa1 == states ma2 &&
        length (final ma1) == length (final ma2) && equalPermutation ma1 ma2
  where
    equalPermutation (DFA s i t1 f1) (DFA _ _ t2 f2) =
        let ps = map (i :) $ List.permutations $ s List.\\ [i]
         in any (\ss ->
                     let m = Map.fromList (zip s ss)
                      in map (m Map.!) f1 == f2 && kvTransMap (m Map.!) t1 == t2)
                ps
    equalPermutation _ _ = error "equalPermutation can only be called on DFAs"

instance Show Automaton where
    show (DFA s i t f) =
        "~DFA~\n" <> "  states: " <> show s <> "\n" <> "  start:  " <> show i <>
        "\n" <>
        "  delta:  " <>
        show' t <>
        "  final:  " <>
        show f <>
        "\n"
    show (NFA s i t f) =
        "~NFA~\n" <> "  states: " <> show s <> "\n" <> "  start:  " <> show i <>
        "\n" <>
        "  delta:  " <>
        show' t <>
        "  final:  " <>
        show f <>
        "\n"

show' :: (Ord a, Ord b, Show a, Show b, Show c) => Map.Map (a, b) c -> String
show' m =
    let kv = Map.toList m
        k = fst $ fst $ head kv
        (grp, rest) = List.break (\x -> (fst . fst) x /= k) kv
     in case rest of
            [] -> showKV grp <> "\n"
            _ ->
                showKV grp <> "\n" <> replicate 10 ' ' <>
                show' (Map.fromList rest)
  where
    showKV ((a, b):tl) = show a <> " -> " <> show b <> " " <> showKV tl
    showKV _ = ""

----------------------
-- Helper functions --
----------------------
-- states accessor
states :: Automaton -> [State]
states (NFA s _ _ _) = s
states (DFA s _ _ _) = s

-- start state accessor
start :: Automaton -> State
start (NFA _ s _ _) = s
start (DFA _ s _ _) = s

-- final states accessor
final :: Automaton -> [State]
final (NFA _ _ _ f) = f
final (DFA _ _ _ f) = f

symbols :: Automaton -> [Symbol]
symbols (NFA _ _ nt _) = filter (/= '_') $ map (snd . fst) $ Map.toList nt
symbols (DFA _ _ dt _) = map (snd . fst) $ Map.toList dt

instance QC.Arbitrary [State] where
    arbitrary = do
        n <- QC.arbitrary :: QC.Gen Int
        pure ["s" <> show i | i <- [0 .. n]]
    shrink = List.inits

replicateEachGen :: (QC.Arbitrary a) => [a] -> QC.Gen [a]
replicateEachGen [] = pure []
replicateEachGen (h:t) = do
    n <- QC.choose (1, 5)
    tl <- replicateEachGen t
    pure $ replicate n h <> tl

instance QC.Arbitrary Automaton where
    arbitrary = do
        states' <-
            do n <- QC.choose (1, 10) :: QC.Gen Int
               pure $ ["s" <> show i | i <- [0 .. n]]
        final' <- QC.sublistOf states'
        symbols' <- QC.listOf1 QC.arbitrary `QC.suchThat` notElem eps
        states'' <- replicateEachGen states'
        let start' = "s0"
        perm <- QC.shuffle states''
        deltaList <-
            QC.sublistOf (zip (zip states' symbols') perm) `QC.suchThat` (/= [])
        pure $ DFA states' start' (Map.fromList deltaList) final'
    shrink = undefined
    -- shrink states & adjust final states and delta accordingly
