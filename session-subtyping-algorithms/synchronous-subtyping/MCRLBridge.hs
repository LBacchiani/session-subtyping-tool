module MCRLBridge where

import Data.Tree as T
import Data.List as L

data MuDuo = SAnd | SOr
data MuModal = Box | Diamond

data RegFormula = Action String     -- <a>
                | LAction [String]  -- <a+b>
                | LActionP [String] -- <(a+b)+>
                | LActionS [String] -- <(a+b)*>
                | SActionS [String] -- <(a.b.c...)*>
                | SAction [String] -- <a.b.c...>
                | RBool Bool

-- data ExprFormula = EBool Bool
--                  | EAnd ExprFormula ExprFormula
--                  | EOr ExprFormula ExprFormula
  
data StateFormula = MuFix String StateFormula
                  | NuFix String StateFormula
                  | Duo MuDuo StateFormula StateFormula
                  | Mod MuModal RegFormula StateFormula
                  | SBool Bool
                  | SVar String -- Rec var
                  | Clause MuDuo [StateFormula]
                  | Not StateFormula
                    


mkAction :: String -> RegFormula
mkAction s = Action s


printRegFormula :: RegFormula -> String
printRegFormula (Action s) = s
printRegFormula (LAction as) = "("++(choiceActions_ "+" as)++")"
printRegFormula (LActionP as) = "("++(choiceActions_ "+" as)++")+"
printRegFormula (LActionS as) = "("++(choiceActions_ "+" as)++")*"
printRegFormula (SAction as) = "("++(choiceActions_ "." as)++")"
printRegFormula (SActionS as) = "("++(choiceActions_ "." as)++")*"
printRegFormula (RBool True) = "true"
printRegFormula (RBool False) = "false"

choiceActions_ op (x:y:xs) = x++op++(choiceActions_ op (y:xs))
choiceActions_ op [x] = x
choiceActions_ op [] = [] 


printMuDuo :: MuDuo -> String
printMuDuo SAnd = " && "
printMuDuo SOr = " || "

printStateFormula :: StateFormula -> String
printStateFormula (MuFix s e) = "mu "++s++" . ("++(printStateFormula e)++")"
printStateFormula (NuFix s e) = "nu "++s++" . ("++(printStateFormula e)++")"
printStateFormula (Duo op e1 e2) = "(("++(printStateFormula e1)++")"++(printMuDuo op)++"("++(printStateFormula e2)++"))"
printStateFormula (Mod Box r s) = "["++printRegFormula r++"]("++(printStateFormula s)++")"
printStateFormula (Mod Diamond r s) = "<"++printRegFormula r++">("++(printStateFormula s)++")"
printStateFormula (SBool True) = "true"
printStateFormula (SBool False) = "false"
printStateFormula (SVar s) = s
printStateFormula (Not s) = "(! ("++(printStateFormula s)++"))"
printStateFormula (Clause op l) = "("++helper l++")"
  where helper (x:y:xs) = "("++(printStateFormula x)++")"++(printMuDuo op)++(helper (y:xs))
        helper [x] = "("++printStateFormula x++")"
        helper [] = []


toDataTree :: StateFormula -> Tree String
toDataTree (MuFix s e) = Node ("Mu "++s) [toDataTree e]
toDataTree (NuFix s e) = Node ("Nu "++s) [toDataTree e]
toDataTree (Duo op e1 e2) = Node ("(("++printMuDuo op++"))") [toDataTree e1, toDataTree e2]
toDataTree (Mod Box r s) = Node ("["++printRegFormula r++"]") [toDataTree s]
toDataTree (Mod Diamond r s) = Node ("<"++printRegFormula r++">") [toDataTree s]
toDataTree (SBool True) = Node "true" []
toDataTree (SBool False) = Node "false" []
toDataTree (SVar s) = Node s []
toDataTree (Clause op l@(x:y:xs)) = Node ("(("++printMuDuo op++"))") (L.map toDataTree l)
toDataTree (Clause op [x]) =  toDataTree x
toDataTree (Not e) = Node "Not" [toDataTree e]

printMuFormula :: StateFormula -> String
printMuFormula f = drawTree $ toDataTree f
