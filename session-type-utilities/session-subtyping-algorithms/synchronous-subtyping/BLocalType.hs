{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, BangPatterns #-}
module BLocalType where


import Bound
import Control.Applicative
import Control.Monad (ap)
import Prelude.Extras
import Data.Foldable
import Data.Traversable as F
import qualified Parser as P
import Data.Set as S
import Data.List as L
import Data.Map as M
import Data.Ord

-- DEBUG
import System.IO.Unsafe
import Debug.Trace


data MLocalType a = Act P.Direction String (MLocalType a)    -- Send/Receive prefix
                  | Rec (Scope () MLocalType a)              -- Recursive def
                  | V a                                      -- Recursive call
                  | End                                      -- End
                  | Choice P.Direction [MLocalType a]
                  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)


instance Eq1 MLocalType
instance Ord1 MLocalType
instance Show1 MLocalType
instance Read1 MLocalType
instance Applicative MLocalType where pure = V; (<*>) = ap

instance Monad MLocalType where
  return = V
  (Act dir s t) >>= f = (Act dir s (t >>= f))
  (Rec e) >>= f = (Rec (e >>>= f))                     ----- >>>= is from Bound
  End >>= f = End
  (Choice dir xs) >>= f = (Choice dir (L.map (>>= f) xs))
  (V v) >>= f = f v



type GoalMap = Map (MLocalType String) (Set (MLocalType String))
type Goals = (GoalMap, GoalMap)

emptyGoals :: Goals
emptyGoals = (M.empty, M.empty)


-- g1 :: Map Subtype=Rec X.T  ---> (Set Supertype)
-- g2 :: Map Supertype=Rec X.T  ---> (Set Subtype)
--
lookupGoal :: Bool -> Goals -> MLocalType String -> MLocalType String -> Bool
lookupGoal left (g1, g2) t1 t2 = if left 
                                 then (search g1 t1 t2) || (search g2 t2 t1) 
                                 else (search g2 t2 t1) || (search g1 t1 t2) 
  where search g t1 t2 =  case M.lookup t1 g of
          Just t ->  t2 `S.member` t
          Nothing -> False
          
              
insertGoal :: GoalMap -> MLocalType String -> MLocalType String  -> GoalMap
insertGoal goals t1 t2 = M.insertWith (S.union) t1 (S.singleton t2) goals




subtyping ::  Goals -> MLocalType String -> MLocalType String -> Bool
subtyping goals End End = 
  True
subtyping goals@(g1, g2) t1@(Rec t') t2 = 
  (lookupGoal True goals t1 t2)
  ||
  (subtyping (insertGoal g1 t1 t2, g2) (instantiate1 t1 t') t2 )
subtyping goals@(g1, g2) t1 t2@(Rec t') = 
  (lookupGoal False goals t1 t2)
  ||
  (subtyping (g1, insertGoal g2 t2 t1) t1 (instantiate1 t2 t') )
subtyping goals (Choice dir xs) (Choice dir' ys) = 
  let pairs = getPairs xs
      pairs' = getPairs ys
      prod = [(t,t') | (m,t) <- pairs, (m',t') <- pairs', m == m']
      mkset s =  S.fromList (L.map fst s)
      checkSub = if (dir == P.Send)
                 then (mkset pairs)  `isSubsetOf` (mkset pairs')
                 else (mkset pairs') `isSubsetOf` (mkset pairs)
  in
   (dir == dir') 
   && checkSub
   && (L.and $ L.map (uncurry $ subtyping goals) prod)
subtyping goals (Act dir s lt) sup = 
  subtyping goals (Choice dir [Act dir s lt]) sup 
subtyping goals sub (Act dir s lt) =
  subtyping goals sub (Choice dir [Act dir s lt])
subtyping _ End _ = False
subtyping _ _ End = False
-- subtyping _ t1 t2 = error $ ("\nSUB: "++(show t2)++"\nSUP: "++(show t2))



getPairs :: [MLocalType a] -> [(String, MLocalType a)]
getPairs ((Act P.Send s lt):xs) = (P.string2send s, lt):(getPairs xs)
getPairs ((Act P.Receive s lt):xs) = (P.string2receive s, lt):(getPairs xs)
getPairs [] = []
getPairs lt = error $ "Ill-formed type: "




translate :: P.LocalType -> MLocalType String
translate (P.Act dir s t) = Act dir s (translate t)
translate (P.Var s) = V s
translate (P.Rec s t) = Rec $ abstract1 s (translate t)
translate P.End = End
translate (P.Choice dir xs) = Choice dir (L.map translate xs)



mksubtyping :: P.LocalType -> P.LocalType -> Bool
mksubtyping t1 t2 = let !t1' = translate t1
                        !t2' = translate t2
                    in subtyping emptyGoals t1' t2'
