module GenFormula where

import Parser
import GenModel
import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Foldable as F
import Data.Either as E
import MCRLBridge

-- DEBUG
import System.IO.Unsafe
import Debug.Trace

type Stack = [[String]]

type Pops = Map String Bool


choiceActions :: [String] -> RegFormula
choiceActions [] = RBool False
choiceActions acts = (LAction acts)

-- The addition of < NuFix "temp" $ > is to speed up mCRL2
-- see discussion on mailing list
makeFormula :: [(String, LocalType)] -> MuDuo -> MuModal -> (LocalType -> StateFormula) -> StateFormula
makeFormula list connector box cont = 
  case list of 
    [] -> SBool True
    [(a,t)] -> Mod box (mkAction a) (NuFix "temp" $ cont t)
    xs -> Clause connector (L.map (\(a,t) -> Mod box (mkAction a) (NuFix "temp" $ cont t)) xs)

subtype :: Direction -> [String] -> LocalType -> StateFormula
subtype star allactions lt =
  case lt of
    (Choice dir xs) -> if dir==star 
                       then makeFormula (getPairs xs) SAnd Diamond (subtype star allactions)
                       else let pairs = (getPairs xs)
                                otheractions = ((L.\\) allactions (L.map fst pairs))
                            in Clause SAnd
                               [ makeFormula pairs SAnd Box (subtype star allactions)
                               , makeFormula pairs SOr Diamond (\x -> SBool True)
                               , if (L.null otheractions)
                                 then (SBool True)
                                 else Mod Box (choiceActions otheractions) (SBool False)
                               ]
    --
    (Rec s lt') -> NuFix s (subtype star allactions lt' )
    --
    (Var s) -> (SVar s)
    --
    End ->  (Mod Box (choiceActions allactions) (SBool False))
    --
    (Act dir s lt') -> subtype star allactions (Choice dir [lt])
    
    
    
    
    
    

-- --------------------><--------------------><-----------------------

finalState :: [String] -> [String] -> StateFormula
finalState parent allactions = 
  (Mod Box (choiceActions allactions) (SBool False))
  
genParentFormula :: [String] ->  [String] -> [String] -> StateFormula
genParentFormula (x:xs) allrcvs allsends =
  Duo SAnd
    (Clause SOr 
     [ (Mod Diamond (SAction (x:xs)) (SBool True))
     , (Mod Box (LAction $ L.map act2parent allrcvs) (SBool False))
     ]
    )
    (Mod Box (mkAction "ROOT") (SBool False)) 
genParentFormula [] _ _ = SBool True



choiceActionsStar :: [String] -> RegFormula
-- choiceActionsStar [] = error $ "No actions in <(...)+>!"
choiceActionsStar [] = RBool False
choiceActionsStar acts = (LActionS acts)

choiceActionsPlus :: [String] -> RegFormula
-- choiceActionsPlus [] = error $ "No actions in <(...)+>!"
choiceActionsPlus [] = RBool False
choiceActionsPlus acts = (LActionP acts)


                  




makeAsyncFormula :: [(String, LocalType)] -> Stack -> MuModal -> (Stack -> LocalType -> StateFormula) -> StateFormula
makeAsyncFormula list stack box fun = 
  case list of 
    [] -> SBool True
    [(a,t)] ->  (Mod box (mkAction a) (fun stack t))
    xs ->   Clause SAnd (L.map (\(a,t) -> Mod box (mkAction a) (fun stack t)) xs)




mkConjunction :: [a] -> MuDuo -> (a -> StateFormula) -> StateFormula
mkConjunction (x:y:xs) op fun = Clause op (L.map fun (x:y:xs))
mkConjunction [x] op fun = (fun x)
mkConjunction [] _ _ = SBool True




unfoldTypei :: Int -> Environment -> LocalType -> LocalType 
unfoldTypei b env (Rec s lt) = let map = M.insertWith (\(i,t) -> \(j,t') -> (1+j,t)) s (0,lt) env
                                   (k,lt') = map!s
                                   unf = (unfoldTypei b map lt)
                                in if k <= b
                                   then unf
                                   else Rec s unf
unfoldTypei b env (Var s) = let (i,lt) = (env!s)
                            in if b < i
                               then (Var s)
                               else unfoldTypei b env (Rec s lt)
unfoldTypei b env (Act Send s lt) = Act Send s (unfoldTypei b env lt)
unfoldTypei b env (Act Receive s lt) = Act Receive s (unfoldTypei b env lt)
unfoldTypei b env End = End
unfoldTypei b env (Choice Send xs) = Choice Send $ L.map (unfoldTypei b env) xs 
unfoldTypei b env (Choice Receive xs) = Choice Receive $ L.map (unfoldTypei b env) xs 



