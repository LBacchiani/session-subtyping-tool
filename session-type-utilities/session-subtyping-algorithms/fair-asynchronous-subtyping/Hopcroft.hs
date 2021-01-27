{-# LANGUAGE TypeFamilies,
  FlexibleContexts,FlexibleInstances #-}


-- from https://github.com/m4b/avant-lexeme/blob/master/Hopcroft.lhss
module Hopcroft(hopcroft) where

import FiniteStateAutomata(FSA(..), DFA'(..))

import Data.Maybe(fromJust, isJust, isNothing)
import qualified Data.Map as M
import qualified Data.Set as S

hopcroft :: (Ord a, Show a) => DFA' a -> DFA' a
hopcroft dfa = hopcroft' (dropUnreachable dfa) 
         parts partMap where
  accept'   = accepting dfa
  notAccept = S.difference (states dfa) accept'
  parts     = S.fromList [accept', notAccept]
  partMap   = toPartitionMap parts
  
hopcroft' :: (Ord a, Show a) => 
          DFA' a -> S.Set (S.Set Int) ->
           M.Map Int Int -> DFA' a
hopcroft' dfa set eqMap = 
          if done then dfa' else recurse where
  done        = consistent' == S.empty
  consistent' = consistent dfa eqMap . S.toList $ set
  dfa'        = buildDFA dfa eqMap
  recurse     = hopcroft' dfa set' eqMap' where
    set'   = S.union (S.delete consistent' set)
     (partition dfa eqMap consistent')
    eqMap' = toPartitionMap set'

consistent :: (Ord a, Show a) => 
           DFA' a -> M.Map Int Int -> 
           [S.Set Int] -> S.Set Int
consistent _ _ [] = S.empty
consistent dfa eqMap (s:ss') = 
           if continue then recurse else s where 
  continue = (isConsistent dfa eqMap (S.toList s))
  recurse  = consistent dfa eqMap ss'

buildDFA :: (Ord a, Show a) => DFA' a ->
          M.Map Int Int -> DFA' a
buildDFA dfa eqMap = DFA' alphabet' ss' accept' st' where
  alphabet'   = alphabet dfa
  ss'         = M.fromList oldStates
  accept'     = S.map lookup' (accepting dfa)
  st'         = lookup' (start dfa)
  newStates   = S.toList . S.fromList . M.elems $ eqMap
  oldStates   = zip newStates . 
   map (updateState dfa eqMap) . map check $ newStates
  check ns    = M.keys . M.filter (== ns) $ eqMap
  lookup'      = (eqMap M.!)

updateState :: (Ord a, Show a) => 
            DFA' a -> M.Map Int Int ->
             [Int] -> M.Map a Int
updateState dfa eqMap oldStates = update where
  update = M.map (eqMap M.!) . 
   M.unions . map fromJust . 
   filter isJust . map lookup' $ oldStates
  lookup' = flip M.lookup (trans dfa)

-- Builds a partition map for equivalence look up
toPartitionMap :: S.Set (S.Set Int) -> M.Map Int Int
toPartitionMap = toPartitionMap' 0 M.empty . S.toList 
               where
  toPartitionMap' _ acc []         = acc
  toPartitionMap' next acc (s:ss') = 
   toPartitionMap' (next+1) acc' ss' where
    acc'   = S.fold insert acc s
    insert = flip M.insert next

-- Partitions a given equivalence group
partition :: (Ord a, Show a) => DFA' a -> 
          M.Map Int Int -> S.Set Int -> S.Set (S.Set Int)
partition dfa parts toPart = 
          partition' S.empty dfa parts (S.toList toPart) 
          where
  partition' acc _ _ [] = acc
  partition' acc dfa parts (s:ss) = 
   partition' acc' dfa parts ss' where
    acc'    = S.insert set acc
    sMap    = eqMap s
    matches = filter ((sMap ==) . eqMap) ss
    set     = S.fromList (s:matches)
    ss'     = filter elems ss
    elems x = not (S.member x set)
    eqMap x = eqMap' where
      map'   = M.lookup x (trans dfa)
      eqMap' = 
       if isNothing map' then 
         M.empty 
       else 
         equivalenceMap parts . fromJust $ map'

-- Determines if a set of states all have the same edges
isConsistent :: (Ord a, Show a) => 
             DFA' a -> M.Map Int Int -> [Int] -> Bool
isConsistent _ _ [] = True
isConsistent dfa partitions (s:ss) = 
             isConsistent' dfa partitions eqMap ss where
  map = M.lookup s (trans dfa)
  eqMap = 
   if isNothing map then 
     M.empty 
   else 
     equivalenceMap partitions . fromJust $ map

equivalenceMap :: M.Map Int Int -> 
                  M.Map a Int -> M.Map a Int
equivalenceMap partitions map' = 
               M.mapWithKey updateKey map' where
  updateKey _ v = partitions M.! v

isConsistent' :: (Ord a, Show a) => 
              DFA' a -> M.Map Int Int -> 
              M.Map a Int -> [Int] -> Bool
isConsistent' _ _ _ [] = True

isConsistent' dfa partitions eqMap (s:ss') = 
              if consistent' then 
               recurse 
              else
                False where
  consistent' = map' == eqMap
  mMap = M.lookup s (trans dfa)
  map' = 
   if isNothing mMap then
    M.empty 
   else equivalenceMap partitions . fromJust $ mMap
  recurse = isConsistent' dfa partitions eqMap ss'

-- Removes all unreachable states in a DFA'
dropUnreachable :: (Ord a, Show a) => DFA' a -> DFA' a
dropUnreachable dfa = dropUnreachable' set set dfa where 
  set = S.singleton $ start dfa

dropUnreachable' :: (Ord a, Show a) => 
                 S.Set Int -> S.Set Int -> DFA' a -> DFA' a
dropUnreachable' reachable_states new_states dfa =
                  if done then dfa' else recurse where
  reachable'        = 
   S.unions . S.toList . S.map (reachable dfa) $ new_states
  new_states'       = 
   S.difference reachable' reachable_states
  reachable_states' = 
   S.union reachable_states new_states'
  recurse           = 
   dropUnreachable' reachable_states' new_states' dfa 
  dfa'              = 
   updateDFA dfa reachable_states'
  done              = new_states' == S.empty

updateDFA :: (Ord a, Show a) => 
          DFA' a -> S.Set Int -> DFA' a
updateDFA dfa reachable_states = 
          DFA' alphabet' trans' accept' start' where
  unreachable_states = 
   S.difference (states dfa) reachable_states
  accept'            = 
   S.difference (accepting dfa) unreachable_states
  alphabet'          = alphabet dfa
  start'             = start dfa
  trans'             = M.filterWithKey removeKey (trans dfa)
  removeKey k _      = S.member k reachable_states

reachable :: (Ord a, Show a) => DFA' a -> Int -> S.Set Int
reachable fsa state = S.fromList ns where
  trans'    = M.lookup state (trans fsa)
  ns        = if isNothing trans' then [] else ns'
  ns'       = M.elems . fromJust $ trans'

-- A test DFA that has several unreachable states: [3,4,5,6]
testDFA :: DFA' Char
testDFA = DFA' alpha' ss' accept' st' where
  alpha'  = S.fromList "ab"
  ss'     = 
   M.fromList [(0, trans0),
               (1, trans1),
               (2, trans2),
               (3, trans3)]
  trans0  = M.fromList [('a', 1), ('b', 2)]
  trans1  = M.empty
  trans2  = M.empty
  trans3  = M.fromList [('a', 4), ('b', 5)]  
  accept' = S.fromList [1,2,3,6]
  st'     = 0

-- Tests the removal of unreachable states
testDroppable :: Bool
testDroppable = alphabet' && states' 
                && start' && accepting' where
  alphabet'  = (alphabet dfa) == (S.fromList "ab")
  states'    = (states dfa) == (S.fromList [0,1,2])
  start'     = (start dfa) == 0
  accepting' = (accepting dfa) == (S.fromList [1,2])
  dfa        = dropUnreachable testDFA

-- A test DFA that can be reduced
--  to a single node with two edges
-- it recognizes strings of the language \verb=(a|b)*=
testDFA' :: DFA' Char
testDFA' = DFA' alpha' ss' accept' st' where
  alpha'  = S.fromList "ab"
  ss'     = M.fromList 
             [(0, trans'),
              (1, trans'),
              (2, trans')]
  trans'  = M.fromList [('a', 1), ('b', 2)]
  accept' = S.fromList [0,1,2]
  st'     = 0
  
-- Tests that hopcroft reduces testDFA' to a minimal dfa  
testHopcroft :: Bool
testHopcroft = alphabet' && states'
               && accepting' && trans' where
  alphabet'  = (alphabet dfa) == (S.fromList "ab")
  states'    = (states dfa) == (S.fromList [start'])
  start'     = (start dfa)
  accepting' = (accepting dfa) == (S.fromList [start'])
  trans'     = (trans dfa) == (M.fromList [(start', trans0)])
  trans0     = M.fromList [('a', start'), ('b', start')]
  dfa        = hopcroft testDFA'
  
testPartition :: Bool
testPartition = partition' == correctPartition where
  partition'       = partition dfa parts toPart
  correctPartition = S.fromList [s1,s2,s3] 
  s1               = S.fromList [1,2,5]
  s2               = S.fromList [3]
  s3               = S.fromList [4,7]
  parts            = M.fromList 
                      [(0,0),(6,0),
                       (1,1),(2,1),
                       (3,1),(4,1),
                       (5,1),(7,1)]
  toPart           = S.fromList [1,2,3,4,5,7]
  dfa              = nonMinimalDFA
  
nonMinimalDFA :: DFA' Char
nonMinimalDFA = DFA' alpha' ss' accept' st' where 
    alpha'  = S.fromList "ab"
    ss'     = M.fromList 
               [(0,trans0),(1,trans1),(2,trans2),
                (3,trans3),(4,trans4),(5,trans5),
                (6,trans6),(7,trans7)]
    trans0  = M.fromList [('a', 1)]
    trans1  = M.fromList [('a', 4), ('b',2)]
    trans2  = M.fromList [('a', 3), ('b',5)]
    trans3  = M.fromList [('b', 1)]
    trans4  = M.fromList [('a', 6), ('b', 5)]
    trans5  = M.fromList [('a', 7), ('b', 2)]
    trans6  = M.fromList [('a', 5)]
    trans7  = M.fromList [('a', 0), ('b', 5)]
    accept' = S.fromList [0, 6]
    st'     = 0
