 {-# LANGUAGE BangPatterns #-}
module GayAndHole where

import Parser
import Data.Set as S
import Data.List as L
import Data.Map as M

-- DEBUG
import System.IO.Unsafe
import Debug.Trace


type GoalMap = Map (LocalType) (Set (LocalType))
type Goals = (GoalMap, GoalMap)

emptyGoals :: Goals
emptyGoals = (M.empty, M.empty)


-- g1 :: Map Subtype=Rec X.T  ---> (Set Supertype)
-- g2 :: Map Supertype=Rec X.T  ---> (Set Subtype)
--
lookupGoal :: Bool -> Goals -> LocalType -> LocalType -> Bool
lookupGoal left (g1, g2) t1 t2 = if left
                                 then (search g1 t1 t2) || (search g2 t2 t1)
                                 else (search g2 t2 t1) || (search g1 t1 t2)
  where search g t1 t2 =  case M.lookup t1 g of
          Just t ->  t2 `S.member` t
          Nothing -> False


insertGoal :: GoalMap -> LocalType -> LocalType  -> GoalMap
insertGoal goals t1 t2 = M.insertWith (S.union) t1 (S.singleton t2) goals



-- maxTerm :: [(LocalType, LocalType)] -> LocalType
-- maxTerm [] = End
-- maxTerm xs = L.maximum $ (L.map fst xs)++(L.map snd xs)

mkGHsubtyping :: LocalType -> LocalType -> Bool
mkGHsubtyping t1 t2 = subtyping emptyGoals t1 t2

subtyping :: Goals -> LocalType -> LocalType -> Bool
subtyping goals End End =
  True
subtyping goals@(g1, g2) t1@(Rec s t') t2 =
  (lookupGoal True goals t1 t2)
  ||
  (subtyping (insertGoal g1 t1 t2, g2) (substitute s t1 t') t2)
subtyping goals@(g1, g2) t1 t2@(Rec s t') =
  (lookupGoal False goals t1 t2)
  ||
  (subtyping (g1, insertGoal g2 t2 t1) t1 (substitute s t2 t'))
subtyping goals (Choice dir xs) (Choice dir' ys) =
  let pairs = getPairs xs
      pairs' = getPairs ys
      prod = [(t,t') | (m,t) <- pairs, (m',t') <- pairs', m == m']
      mkset s =  S.fromList (L.map fst s)
      !checkSub = if (dir == Send)
                  then (mkset pairs)  `isSubsetOf` (mkset pairs')
                  else (mkset pairs') `isSubsetOf` (mkset pairs)
  in
   (dir == dir')
   && checkSub
   && (and $ L.map (uncurry $ subtyping goals) prod)
subtyping goals (Act dir s lt) sup =
  subtyping goals (Choice dir [Act dir s lt]) sup
subtyping goals sub (Act dir s lt) =
  subtyping goals sub (Choice dir [Act dir s lt])
subtyping _ End _ = False
subtyping _ _ End = False
