{-# LANGUAGE TypeFamilies, FlexibleContexts,FlexibleInstances, ConstrainedClassMethods #-}

module FiniteStateAutomata(
                           FSA(..),
                           NFA'(..),
                           NFAMap,
                           DFA'(..),
                           DFAMap,
                           epsilon, ppfsa) where

import qualified Data.Map as M
import qualified Data.Set as S

class (Ord (Alpha f), 
       Show (Alpha f), 
       Show f,
       Show (FSAVal f),
       Listable (FSAVal f)) => FSA f where
  type Alpha f
  type FSAVal f
  alphabet  :: (Ord (Alpha f), Show (Alpha f)) =>
               f -> S.Set (Alpha f)
  accepting :: f -> S.Set Int
  start     :: f -> Int
  trans     :: f -> M.Map Int (FSAVal f)
  states    :: f -> S.Set Int
  states fsa = S.unions [(S.fromList . M.keys . trans $ fsa), 
                         (accepting fsa),
                         (S.fromList . 
                         concatMap sndList . 
                         M.elems . trans $ fsa)]

data DFA' a = DFA' {alpha  :: S.Set a,
                    ss     :: DFAMap a,
                    accept :: S.Set Int,
                    st     :: Int} deriving (Show, Read, Eq)

data NFA' a = NFA' {nalpha  :: S.Set a,
                    nss     :: NFAMap a, 
                    naccept :: S.Set Int,
                    nst     :: Int} 

type DFAMap a = M.Map Int (M.Map a Int)
type NFAMap a = M.Map Int (S.Set (Maybe a, Int))

class (Show (Elem m)) => Listable m where
  type Elem m
  toList :: m -> [(Elem m,Int)]

instance (Show a) => Listable (M.Map a Int) where
  type Elem (M.Map a Int) = a 
  toList = M.toList

instance (Show a) => Listable (S.Set (Maybe a, Int)) where
  type Elem (S.Set (Maybe a, Int)) = Maybe a
  toList = S.toList


sndList :: Listable m => m -> [Int]
sndList = map snd . toList

fsaShow :: (FSA f) => f -> String
fsaShow fsa = "{alphabet=" 
              ++ (show . S.toList . alphabet $ fsa)
              ++ "," ++
              "states=" ++ 
              (show . S.toList . states $ fsa) ++ "," ++
              "start=" ++ (show . start $ fsa) ++ "," ++
              "accepting=" 
              ++ (show . S.toList . accepting $ fsa) 
              ++ "," ++ "trans=" 
              ++ (show . map (filter (/= '"')) . 
                  showTransitions $ fsa)


pettyPrinter :: (FSA f) => f -> IO ()
pettyPrinter fsa = (putStr $ "alphabet=" 
                    ++ (show . S.toList . alphabet $ fsa)
                    ++ "\n" ++
                    "states=" 
                    ++ (show . S.toList . states $ fsa)
                    ++ "\n" ++
                    "start=" ++ (show . start $ fsa)
                    ++ "\n" ++
                    "accepting=" 
                    ++ (show . S.toList . accepting $ fsa)
                    ++ "\n") >> trans 
    where trans = 
              mapM_ (putStrLn . filter (/= '"')) 
                        $ showTransitions fsa
  
ppfsa :: (FSA f) => f -> IO ()
ppfsa = pettyPrinter

showTransitions :: (FSA f) => f -> [String]
showTransitions fsa = map showTransition .
                      M.toList . trans $ fsa where
  showTransition (from, ts) = (show from) 
        ++ " :: " 
        ++ (show . map showTransition' . toList $ ts) where
    showTransition' (x, to) = (show x) ++ " -> " ++ (show to)
                
instance (Ord a, Show a) => FSA (DFA' a) where
  type Alpha (DFA' a) = a
  type FSAVal (DFA' a) = (M.Map a Int)
  alphabet = alpha
  accepting = accept
  start = st
  trans = ss

epsilon :: Maybe a              
epsilon = Nothing

instance (Ord a, Show a) => FSA (NFA' a) where
  type Alpha (NFA' a) = a
  type FSAVal (NFA' a) = (S.Set (Maybe a, Int))
  alphabet = nalpha
  accepting = naccept
  start = nst
  trans = nss

instance (Ord a, Show a) => Show (NFA' a) where
  show nfa = "NFA " ++ (fsaShow nfa)
  
simpleNFA :: NFA' Char  
simpleNFA = NFA' alpha states accepting start where
  alpha = S.fromList ['a','b']
  states = M.fromList 
           [(0, S.fromList [(Just 'a', 1)]),
            (1, S.fromList [(Just 'b', 0), (epsilon, 2)])]
  start = 0
  accepting = S.fromList [2]

simpleDFA :: DFA' Char
simpleDFA = DFA' alpha states accepting start where
  alpha = S.fromList ['a','b','c']
  states = M.fromList 
           [(0, M.fromList [('a', 1)]),
            (1, M.fromList [('b', 0), ('c', 2)])]
  start = 0
  accepting = S.fromList [2]

deadStateDFA :: DFA' Char
deadStateDFA = DFA' alpha states accepting start where
  alpha = S.fromList "ab"
  states = 
   M.fromList [(0, trans0), (1, trans1), (2, trans2)] where
    trans0 = M.fromList [('a', 1), ('b', 2)]
    trans1 = M.fromList [('b', 3)]
    trans2 = M.fromList [('a', 3)]
  accepting = S.fromList [1, 2]
  start = 0

