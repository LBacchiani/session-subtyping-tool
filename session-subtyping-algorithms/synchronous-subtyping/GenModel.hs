module GenModel where

import Parser
import Data.List as L
import Data.Foldable as F
import Data.Map as M




getRecursions :: LocalType -> Map String LocalType
getRecursions lt = let (t, map) = helper lt
                   in M.insert "LINIT" t map
  where 
    helper :: LocalType -> (LocalType, Map String LocalType)
    helper (Rec var lt) = let (lt', equations) = helper lt
                          in (Var var, M.insert var lt' equations)
    helper (Var var) = (Var var, M.empty)
    helper (Act dir s lt) = let (lt', equations) = helper lt
                            in (Act dir s lt', equations)
    helper End = (End, M.empty)
    helper (Choice dir xs) = let cont = L.map helper $ snd $ (L.mapAccumL (\acc x -> (acc+1, (rename [] acc x))) 0 xs)
                                 merge = M.unions $ L.map snd cont
                             in (Choice dir (L.map fst cont), merge)

-- rename only BOUND variables
rename :: [String] -> Int -> LocalType -> LocalType
rename bound i (Rec var lt) = Rec (var++(show i)) (rename (var:bound) i lt)
rename bound i (Var var) = if var `L.elem` bound 
                           then Var (var++(show i))
                           else Var var
rename bound i (Act dir s lt) = Act dir s (rename bound i lt)
rename bound i (Choice dir xs) = Choice dir (L.map (rename bound i) xs)
rename bound i End = End


genEquations :: Map String LocalType -> String
genEquations maps = helper (M.toList maps)
  where helper (x:y:xs) = genLine x
                        ++(helper (y:xs))
        helper [x] = genLine x
        helper [] = []
        -- 
        genLine (var, lt) = var++" = "++genType lt++";\n"
        --
        genType End = "delta"
        genType (Var var) = var
        genType (Act dir s lt) = (if dir == Receive 
                                  then string2receive s
                                  else string2send s)++" . "++(genType lt)
        genType (Choice dir xs) = "("++(printChoice xs )++")"
        --
        printChoice (x:y:xs) = "("++(genType x)++")"++" + "++(printChoice (y:xs))
        printChoice [x] = "("++(genType x)++")"
        printChoice [] = []
          

genModel :: [String] -> [String] -> LocalType -> String
genModel sends receives lt = 
  (if L.null sends &&  L.null receives
   then ""
   else "act"     
  )
  ++(if L.null sends 
     then "" 
     else "\n"++(genActions sends)++"; \t % SEND ACTIONS"
    )
  ++(if L.null receives
     then ""
     else "\n"++(genActions receives)++"; \t % RECEIVE ACTIONS"
    )
  ++"\n\nproc\n"++(genEquations (getRecursions lt))
  ++"\ninit\nLINIT;"
  where genActions (x:y:xs) = x++", "++(genActions (y:xs))
        genActions [x] = x
        genActions [] = ""
        
