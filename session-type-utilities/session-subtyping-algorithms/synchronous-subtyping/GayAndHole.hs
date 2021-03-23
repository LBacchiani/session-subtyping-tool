{-# LANGUAGE BangPatterns #-}
module GayAndHole where

import Automata
import Parser
import Data.Set as S
import Data.List as L
import Data.Map as M

-- DEBUG
import System.IO.Unsafe
import Debug.Trace


type Step = ((State,State),((Label,(State,State))))
type Simulation = [Step]


subtyping :: Machine -> Machine -> ([(State,State)],Simulation)
subtyping m1 m2 = helper m1 m2 (tinit m1) (tinit m2) []
  where helper m1 m2 curr1 curr2 marked
          | (curr1,curr2) `L.elem` marked = ([],[])
          | (isFinal m1 curr1) && (isFinal m2 curr2) = ([],[])
          | (isInput m1 curr1) && (isInput m2 curr2) =
            let
              l1 = L.map(\(s1,(l,s2)) -> l) (L.filter(\(s1,(l,s2)) -> s1 == curr1)(transitions m1))
              l2 = L.map(\(s1,(l,s2)) -> l) (L.filter(\(s1,(l,s2)) -> s1 == curr2)(transitions m2))
            in case (S.fromList l2) `isSubsetOf` (S.fromList l1) of
              True -> L.foldl(\(err,sim) x -> let target1 = targetState (transitions m1) curr1 x
                                                  target2 = targetState (transitions m2) curr2 x
                                                  (currErr,currSim) = helper m1 m2 target1 target2 ((curr1,curr2):marked)
                                              in (err ++ currErr, ((curr1,curr2),(x,(target1,target2))):(sim ++ currSim))) ([],[]) l2
              False -> ([(curr1,curr2)],[])
          | (isOutput m1 curr1) && (isOutput m2 curr2) =
            let
              l1 = L.map(\(s1,(l,s2)) -> l) (L.filter(\(s1,(l,s2)) -> s1 == curr1)(transitions m1))
              l2 = L.map(\(s1,(l,s2)) -> l) (L.filter(\(s1,(l,s2)) -> s1 == curr2)(transitions m2))
            in case (S.fromList l1) `isSubsetOf` (S.fromList l2) of
              True -> L.foldl(\(err,sim) x -> let target1 = targetState (transitions m1) curr1 x
                                                  target2 = targetState (transitions m2) curr2 x
                                                  (currErr,currSim) = helper m1 m2 target1 target2 ((curr1,curr2):marked)
                                              in (err ++ currErr, ((curr1,curr2),(x,(target1,target2))):(sim ++ currSim))) ([],[]) l1
              False -> ([(curr1,curr2)],[])
          | otherwise = ([(curr1,curr2)],[])

targetState :: [Transition] -> State -> Label -> State
targetState transitions currState currTrans = head (L.map(\(s1,(l,s2)) -> s2) (L.filter(\(s1,(l,s2)) -> (s1 == currState) && (l == currTrans)) (transitions)))


-------------------------------------------------------------PRINTING STUFF------------------------------------------------------------------


printSimulation :: (State,State) -> [(State,State)] -> Simulation -> String
printSimulation initials blockings sim = case sim of
  [] -> "digraph CTs {\n subgraph cluster {\n}\n}"
  _ -> "digraph CTs {\n subgraph cluster {\n" ++ printNodes initials blockings sim ++ printTransitions sim ++ "}\n}\n"

printNodes :: (State,State) -> [(State,State)] -> Simulation -> String
printNodes initials blockings sim = helper initials blockings sim [] ""
  where helper initials blockings (x:xs) seen toPrint
          | ((fst x) `L.elem` seen) && ((snd (snd x)) `L.elem` seen) = if checkLast xs then toPrint else helper initials blockings xs seen toPrint
          | (fst x) `L.elem` seen = let newOut = toPrint ++ printSingleNode initials (snd (snd x)) blockings in if checkLast xs then newOut else helper initials blockings xs ((fst x):seen) newOut
          | (snd (snd x)) `L.elem` seen = let newOut = toPrint ++ printSingleNode initials (fst x) blockings in if checkLast xs then newOut else helper initials blockings xs ((snd (snd x)):seen) newOut
          | otherwise = let newOut = toPrint ++ printSingleNode initials (fst x) blockings ++ printSingleNode initials (snd (snd x)) blockings in if checkLast xs then newOut else helper initials blockings xs ([(fst x),(snd (snd x))] ++ seen) newOut


printSingleNode :: (State,State) -> (State,State) -> [(State,State)] -> String
printSingleNode initials currents blockings = case blockings of
        [] -> "  node" ++ (fst currents) ++ (snd currents) ++ "[shape = \"rectangle\" color =\"black\""  ++ (if initials == currents then "penwidth = 3" else "") ++ " label=<<table color='white'><tr><td color='black'><b>" ++ (fst currents) ++ "</b></td><td color='black'><b><font color='black'>" ++ (snd currents) ++ "</font></b></td></tr></table>>];\n"
        _ -> "  node" ++ (fst currents) ++ (snd currents) ++ "[shape = \"rectangle\" " ++ (if currents `L.elem` blockings then "color =\"red\"" else "color =\"black\"")  ++ (if initials == currents then " penwidth = 3" else "") ++ " label=<<table color='white'><tr><td color='black'><b>" ++ (fst currents) ++ "</b></td><td color='black'><b><font color='black'>" ++ (snd currents) ++ "</font></b></td></tr></table>>];\n"

printTransitions :: Simulation -> String
printTransitions sim = helper sim ""
  where helper (x:xs) toPrint
          | checkLast xs = " node" ++ (fst (fst x)) ++ (snd (fst x)) ++ "->" ++ "node" ++  (fst (snd (snd x))) ++ (snd (snd (snd x))) ++ "[label=\"" ++ printTransitionLabel (fst (snd x)) ++ "\"]\n"
          | otherwise = helper xs toPrint ++ " node" ++ (fst (fst x)) ++ (snd (fst x)) ++ "->" ++ "node" ++  (fst (snd (snd x))) ++ (snd (snd (snd x))) ++ "[label=\"" ++ printTransitionLabel (fst (snd x)) ++ "\"]\n"

printTransitionLabel :: Label -> String
printTransitionLabel l = case l of
          (Send, name) -> "!" ++ name
          (Receive, name) -> "?" ++ name

checkLast :: Simulation -> Bool
checkLast [] = True
checkLast [x] = False
checkLast (x:xs) = False

