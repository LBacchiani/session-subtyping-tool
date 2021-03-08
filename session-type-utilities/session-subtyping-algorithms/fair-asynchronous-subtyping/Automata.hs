module Automata where

import Data.List as L
import Data.Set as S
import Data.Map as M
import Data.Char (toUpper)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
-- import Data.Hashable

import qualified FiniteStateAutomata as FS
import qualified Hopcroft as H
import qualified Data.Text as T
import System.Process


import Parser

-- DEBUG
import System.IO.Unsafe
import Debug.Trace

type Message = String -- T.Text -- Int --
type Label = (Direction, Message)
type State = String
type Transition = (State, (Label, State))
type Element = ((State, State), [(State, Label)])

-- type IdxMachine = (State, [Label])
type Pair = (State, State, [(State, Label)])




data Machine = Machine
               { states :: [State]
               , tinit :: State
               , transitions :: [Transition]
               , accepts :: [State]
               } deriving (Show, Eq, Ord)

writeToFile :: FilePath -> String -> IO()
writeToFile path content = do
  createDirectoryIfMissing True $ takeDirectory path

  writeFile path content

printMachine :: Machine -> String
printMachine m =
  let header = "digraph ICTS { \n "
      footer = "}\n "
  in header++(printMachineBody m)++footer


printMachineBody :: Machine -> String
printMachineBody m =
  let nodes = L.map (\x ->
                      "q"++(x)++" [color = blue fontcolor = blue label=\""++(x)
                      ++"\""++
                      (
                        if x==(tinit m)
                        then " penwidth=3 shape = \"rectangle\""
                        else ""
                      )
                      ++"]; \n "
                    ) $ states m
      transi = L.map (\(s,(l,t)) -> "q"++(s)++" -> "++"q"++(t)++" [color = blue fontcolor = blue label=\""++(mprintLabel l)++"\"]"++
                                "; \n ") $ transitions m
  in (foldstring nodes)++(foldstring transi)
  where foldstring s = L.foldr (++) "" s
        mprintLabel (Send,a) = "!"++(a)
        mprintLabel (Receive,a) = "?"++(a)

machine2file :: Machine -> String -> IO ()
machine2file m f = do writeToFile ("tmp/"++f++"_cfsm.dot") (printMachine m)
                      --mkPicture (f++"_cfsm.dot") (f++"_cfsm.svg")

mkPicture :: FilePath -> FilePath -> IO ()
mkPicture file output =
  let cmd = "dot -Tpng "++file++" -o "++output
  in do -- out <- readProcess "bash" ["-c", cmd] []
        out <- readProcess "dot" ["-Tsvg",file,"-o",output] []
        return ()

genState :: String -> (Map String State) -> LocalType -> State
genState s map (Var x) = case M.lookup x map of
  Just y -> y
  Nothing -> error $ "ill formed type"
genState s map (Rec s' t) = genState s map t
genState s map t = s

mkMessage :: String -> Message
mkMessage s = s -- T.pack s




stToUpper :: String -> String
stToUpper = L.map toUpper

isEpsilon :: Direction -> Label -> Bool
isEpsilon dir (d, msg) = dir/= d

numberOfStates :: Machine -> Int
numberOfStates m = length $ states m

makeReverseMachine :: Direction -> Machine -> State -> Machine
makeReverseMachine dir m q = (makeDFA dir) . (removeEpsilon dir) $ rev
  where rev = Machine { states = states m
                      , tinit = q
                      , transitions = L.map (\(x,(y,z)) -> (z,(y,x))) $ transitions m
                      , accepts = [tinit m]
                      }
makeForwardMachine :: Direction -> Machine -> State -> Machine
makeForwardMachine dir m q = (makeDFA dir) . (removeEpsilon dir) $ rev
  where rev = Machine { states = states m
                      , tinit = q
                      , transitions = transitions m
                      , accepts = [tinit m]
                      }


eClosure :: Direction -> Machine -> State -> [State]
eClosure dir m q0 = helper [q0] []
  where helper (q:qs) visited
          | q `elem` visited = helper qs visited
          | otherwise =
            let next = L.map (snd . snd) $
                       L.filter (\(x,(y,z)) -> x == q && isEpsilon dir y) $ transitions m
            in helper (qs++next) (q:visited)
        helper [] visited = visited

dualMachine :: Machine -> Machine
dualMachine m =  Machine { states = states m
                         , tinit = tinit m
                         , transitions = L.map dualTransition $ transitions m
                         , accepts = []
                         }
          where dualTransition (s,(l,t)) = (s, (dualLabel l, t))
                dualLabel (Send, m) = (Receive, m)
                dualLabel (Receive, m) = (Send, m)


removeEpsilon :: Direction -> Machine -> Machine
removeEpsilon dir m = let eclosure p = eClosure dir m p
                          --
                          newtrans p ((x,(y,z)):xs)
                            | x `elem` eclosure p && (not $ isEpsilon dir y) = (p,(y,z)):(newtrans p xs)
                            | otherwise = newtrans p xs
                          newtrans _ [] = []
                          --
                          allnewtrans = L.foldr (++) [] $ L.map (\x -> newtrans x (transitions m)) $ states m
                      in Machine { states = states m
                                 , tinit = tinit m
                                 , transitions = L.nub $ allnewtrans
                                                 ++ (L.filter (\(x,(y,z)) -> not $ isEpsilon dir y) $
                                                     transitions m)
                                 , accepts = L.filter
                                             (\x -> not $ L.null $ intersect (accepts m) (eClosure dir m x))
                                             $ states m
                                 }

eClosureSet :: Direction -> Machine -> [State] -> [State]
eClosureSet dir m xs = L.foldr (++) [] $ L.map (eClosure dir m) xs

move :: Direction -> Machine -> [State] -> Label -> [State]
move dir m xs l = L.map (snd . snd) $
                  L.filter (\(x,(y,z)) -> (x `elem` xs) && l==y)
                  (transitions m)

alphas :: Direction -> Machine -> [State] -> [Label]
alphas dir m states = L.nub $
                      L.map (fst . snd) $
                      L.filter (\(x,(y,z)) -> x `elem` states) $ transitions m




normaliseState :: [State] -> State
normaliseState xs = L.foldr (++) [] xs

normaliseTrans :: ([State], (Label, [State])) -> Transition
normaliseTrans (x,(y,z)) = ((normaliseState x), (y, normaliseState z))


makeDFA :: Direction -> Machine -> Machine
makeDFA dir m =  Machine { states = L.nub $ L.foldr (++) [] $ L.map (\(x,(y,z)) -> [x,z]) newtransitions
                         , tinit = normaliseState initclosure
                         , transitions = newtransitions
                         , accepts = L.nub
                                     $ L.map normaliseState
                                     $ L.filter (\x ->  not $ L.null $ intersect x  (accepts m))
                                     $ L.foldr (++) [] $ L.map (\(x,(y,z)) -> [x,z]) allt
                         }
  where
    initclosure = eClosure dir m (tinit m)
    --
    allt = makeTrans [initclosure] [] []
    newtransitions = L.nub $ L.map normaliseTrans allt
    --
    alphabet = alphas dir m
    --
    makeTrans (current:qs) visited acc
      | current `elem` visited = makeTrans qs visited acc
      | otherwise =
        let alphalist = alphabet current
            --
            addTrans (a:as) = (a,move dir m current a):(addTrans as)
            addTrans [] = []
            --
            translist = addTrans alphalist
            --
        in makeTrans
           (qs++(L.map snd translist))
           (current:visited)
           (acc++(L.map (\x-> (current,x)) translist))
    makeTrans [] visited acc = acc



subsetLggOf :: Machine -> Machine -> Bool
subsetLggOf m1 m2 = helper [] [((tinit m1), (tinit m2))]
  where helper visited ((p,q):xs)
          | (p,q) `elem` visited = helper visited xs
          | otherwise = let psmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==p) $ transitions m1
                            qsmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==q) $ transitions m2
                            qsalpha = L.map fst qsmoves
                            next = [(x,y) | (a,x) <- psmoves, (b,y) <- qsmoves, a==b]
                        in (L.and $ L.map (\(a,t) -> a `elem` qsalpha) psmoves)
                           &&
                           (helper ((p,q):visited) (xs++next))
        helper visited [] = True



-- Can 'p' do a 'tau'
isTau :: Direction -> Machine -> State -> Bool
isTau dir m p =  not $ L.null $ L.filter (\(s,((d,m),t)) -> s==p && d/=dir) $ transitions m

isReceiveLab :: Label -> Bool
isReceiveLab (Receive, l) = True
isReceiveLab _ = False

isInput ::  Machine -> State -> Bool
isInput = isTau Send

isOutput ::  Machine -> State -> Bool
isOutput = isTau Receive


isFinal :: Machine -> State -> Bool
isFinal m p =  L.null $ L.filter (\(s,((d,m),t)) -> s==p) $ transitions m

isAlpha :: Label -> Machine -> State -> Bool
isAlpha l m p = not $ L.null $ L.filter (\(s,(lab,t)) -> s==p && lab==l) $ transitions m


-- List of states one-tau-reachable from p
tauReachable1 :: Direction -> Machine -> State -> [(Label,State)]
tauReachable1 dir m p =
        L.map (snd) $
        L.filter (\(s,((d,m),t)) -> s==p && d/=dir) $ transitions m

-- connect' x y g
--     | x == y    = [[]]
--     | otherwise = [(x,t):path | t <- g!x, path <- connect' t y g]

connect :: Int -> Direction -> Machine -> State -> State -> [[(State, Label)]]
connect bound dir m src trg = L.map (L.map (\(x,y,z) -> (x,y))) $ connectwLabs bound dir m src trg

connectwLabs :: Int -> Direction -> Machine -> State -> State -> [[(State, Label, [Label])]]
connectwLabs bound dir m src trg
  | (bound < 0) = []
  | src == trg = [[]]
  | otherwise = let mysucc = L.filter (\(s,((d,msg),t)) -> s==src && d/=dir) $ transitions m
                    labs q = L.map (fst . snd) $ L.filter (\(s,((d,msg),t)) -> s==q && d/=dir) $ transitions m
                in [(src,l, labs s):path | (s,(l,t)) <- mysucc, path <- connectwLabs (bound-1) dir m t trg]

tauReachableIdx :: Direction -> Machine -> State -> [([(State,Label)],State)]
tauReachableIdx dir m p0 = helper p0 [] []
  where helper p visited path
          | p `elem` visited = [(path, p)]
          | otherwise = let next = tauReachable1 dir m p :: [(Label,State)]
                        in
                         (path,p):(
                           L.foldr (++) [] $
                           L.map (\(x,y) -> helper y (p:visited) (path++[(p,x)])) next
                         )

reachableSendStates :: State -> Machine -> [State]
reachableSendStates q m = helper [q] [] []
  where helper [] seen acc = acc
        helper (q:qs) seen acc
          | q `elem` seen = helper qs seen acc
          | isOutput m q = helper qs (q:seen) (q:acc)
          | isInput m q =  helper (qs++(L.map snd (successors m q ))) (q:seen) acc
          | otherwise = []


-- succ -- UNSAFE!
successor :: Machine -> State -> Label -> State
successor m p l =
  case L.filter (\(s,(lab,t)) -> s==p && l==lab) $ transitions m  of
  [] -> error $ "No successor for "++(show p)++" and "++(show l)
  (x:xs) -> snd $ snd x


successors :: Machine -> State -> [(Label, State)]
successors m p = L.map snd $ L.filter (\(s,(lab,t)) -> s==p) $ transitions m

msuccessor :: Machine -> State -> Label -> Maybe State
msuccessor m p l =
  case L.filter (\(s,(lab,t)) -> s==p && l==lab) $ transitions m of
   (x:xs) -> Just $ snd $ snd x
   [] -> Nothing

barb :: Direction -> Machine -> State -> Set Message
barb dir m p = S.fromList $ L.map (snd . fst . snd) $ L.filter (\(s,((d, lab),t)) -> s==p && d==dir) $ transitions m

inBarb :: Machine -> State -> Set Message
inBarb = barb Receive

outBarb :: Machine -> State -> Set Message
outBarb = barb Send


isTrans :: Machine -> State -> Message -> Bool
isTrans m p msg = not $ L.null $ L.filter (\(s,((dir, lab),t)) -> s==p && msg==lab) $ transitions m


-- Can one reach a state (only with not-taus) such that state can come
-- back to itself only with not-taus
-- U: rec Y . +{ !a ; Y , !b ; ?c ; end }
-- T: ?c ; rec Z . +{ !a ; Z , !b ; end }
ampersand ::  Direction -> Machine -> State -> Bool
ampersand dir m q = not $ selfloop q q []
  where selfloop src trg visited
          | not $ L.null $ L.filter (\(s,((d,m),t)) ->  t==src && s==src && d==dir) $ transitions m = True
          | L.elem src visited = True
          | isTau dir m src = False
          | otherwise = let next = L.map (snd . snd) $
                                   L.filter (\(s,((d,m),t)) ->  s==src && d==dir) $
                                   transitions m
                        in L.or $ L.map (\x -> selfloop x trg (q:visited)) next


isControllable :: Machine -> Bool
isControllable om = transform om (states om) []
  where transform om stateL trans
          | not $ L.null stateL = case isInput om (head stateL) of
            False -> transform om (tail stateL) $ trans ++ L.filter(\x -> (fst x) == (head stateL)) (transitions om)
            _ -> or $ L.map(\x -> transform om (tail stateL) $ x:trans) (L.filter(\x -> (fst x) == (head stateL)) (transitions om))
          | otherwise = isOK Machine { states = states om, tinit = tinit om, transitions = trans, accepts = accepts om }

isOK :: Machine -> Bool
isOK om = helper [] (tinit om) om
  where helper seen q m
          | q `L.elem` seen = endReachable m q
          | isFinal m q = True
          | isInput m q = case successors m q of
                            [(l,t)] -> helper (q:seen) t m
                            ys -> error (show (q, ys))
          | isOutput m q = all (\x -> helper (q:seen) (snd x) m) (successors m q)


endReachable :: Machine -> State -> Bool
endReachable m q = helper [] q
  where helper seen q
          | q `L.elem` seen = False
          | isFinal m q = True
          | otherwise = any (\x -> helper (q:seen) (snd x)) (successors m q)



isStrongControllable :: Machine -> Bool
isStrongControllable m = helper [] (tinit m)
  where helper seen q
          | isFinal m q = True
          | otherwise = let ret = next seen q  in (not $ L.null ret) && (and ret)
        next seen q =  L.map (\x -> helper ((q,x):seen) (snd x)) (L.filter (\x -> not ((q,x) `L.elem` seen)) $ successors m q)

naming :: Machine -> Map State State
naming m = M.fromList $ snd $ mapAccumL (\x y -> (x+1,(y,show x))) 0 (states m)


eqState :: Machine -> State -> State -> Bool
eqState m p q =
  let asuccs r = L.map snd $ L.filter (\(x,y) -> x==r) $ transitions m
  in (S.fromList $ asuccs p) == (S.fromList $ asuccs q)


splitOffFirstGroup :: (a -> a -> Bool) -> [a] -> ([a],[a])
splitOffFirstGroup equal xs@(x:_) = L.partition (equal x) xs
splitOffFirstGroup _     []       = ([],[])

equivalenceClasses _     [] = []
equivalenceClasses equal xs = let (fg,rst) = splitOffFirstGroup equal xs
                              in fg : equivalenceClasses equal rst


minimise :: Machine -> Machine
minimise m = helper (length $ transitions m) m
  where helper s nm = let m' = rename (\x -> (eqNaming nm)!x) nm
                      in if (length $ transitions m') < s
                         then helper (length $ transitions m') m'
                         else m'


eqNaming :: Machine -> Map State State
eqNaming m = let groups = equivalenceClasses (eqState m) $ states m
                 findRep s = head $ head $ L.filter (L.elem s) groups
                 smap = L.map (\x -> (x,findRep x))  $ states m
             in M.fromList smap


rename :: (State -> State) -> Machine -> Machine
rename names m = let -- names = naming $ states m
                     nedges = L.nub $ L.map (\(s,(l,t)) -> (names s,(l,names t))) $ transitions m
                     nodes = L.nub $ L.map (\x -> names x) $ states m
                     naccepts =  L.map (\x -> names x) $ accepts m
                     ninit = names (tinit m)
           in Machine
                       { states = nodes
                       , tinit = ninit
                       , transitions = nedges
                       , accepts = naccepts
                       }

-- rename :: (Map State State) -> Machine -> Machine
-- rename names m = let -- names = naming $ states m
--                      nedges = L.nub $ L.map (\(s,(l,t)) -> (names!s,(l,names!t))) $ transitions m
--                      nodes = L.nub $ L.map (\x -> names!x) $ states m
--                      naccepts =  L.map (\x -> names!x) $ accepts m
--                      ninit = names ! tinit m
--            in Machine
--                        { states = nodes
--                        , tinit = ninit
--                        , transitions = nedges
--                        , accepts = naccepts
--                        }

bisimilar :: Machine -> Machine -> Bool
bisimilar m1 m2 = helper [(tinit m1, tinit m2)] []
  where helper [] seen = True
        helper ((p,q):xs) seen
          | (p,q) `L.elem` seen = helper xs seen
          | otherwise = let pmoves = successors m1 p
                            qmoves = successors m2 q
                            next = [(p',q') | (a,p') <- pmoves, (b,q') <- qmoves, a==b]
                        in (S.fromList (L.map fst pmoves)) == (S.fromList (L.map fst qmoves))
                           &&
                           helper (xs++next) ((p,q):seen)

updateInit :: State -> Machine -> Machine
updateInit q m = Machine { states = states m
                         , tinit = q
                         , transitions = transitions m
                         , accepts = accepts m
                         }

-- removes unused transitions
cleanUp :: Machine -> Machine
cleanUp m = Machine { states = nstates
                    , tinit = tinit m
                    , transitions = nedges
                    , accepts = nstates
                    }
  where nedges = reachableTransitions m (tinit m)
        nstates = nub $ (tinit m):(concat $ L.map (\(s,(l,t)) -> [s,t]) nedges)

reachableTransitions :: Machine -> State -> [Transition]
reachableTransitions m q =  treach [tinit m] [] []
  where treach [] seen acc = acc
        treach (q:qs) seen acc
          | q `L.elem` seen = treach qs seen acc
          | otherwise = let trs = L.map (\e -> (q,e)) (successors m q)
                        in treach (qs++(L.map (snd . snd) trs)) (q:seen) (acc++trs)


type2Machine :: Bool -> String -> LocalType -> Machine
type2Machine nomin s t = let nedges = L.nub $ genEdges ninit M.empty [] t
                             nstates =  L.nub $ ninit:(L.foldr (++) [] $ L.map (\(s,(m,t)) -> [s,t]) nedges)
                             ninit = genState (stToUpper (s++"o")) M.empty t
                             tmpmachine = Machine { states = nstates
                                                  , tinit = ninit
                                                  , transitions = nedges
                                                  , accepts = nstates
                                                  }
                             fun = if nomin
                                   then id
                                   else minimizeHop
                         in fun $
                            rename (\x -> (naming tmpmachine)!x) tmpmachine




  where genEdges prev map acc (Rec s t) = genEdges prev (M.insert s prev map) acc t
        genEdges prev map acc (Act dir s t) =
          let next = genState ((stToUpper prev)++(stToUpper s)) map t
          in (prev, ((dir, mkMessage s), next)):(genEdges next map acc t)
        genEdges prev map acc (Choice dir xs) = L.foldr (++) [] (L.map (genEdges prev map acc) xs)
        genEdges prev map acc (End) = []
        genEdges prev map acc (Var x) = []


minimizeHopState :: State -> Machine -> Machine
minimizeHopState q m = minimizeHop tmpmachine
  where tmpmachine = Machine { states = states m
                             , tinit = q
                             , transitions = transitions m
                             , accepts = accepts m
                             }

minimizeHop :: Machine -> Machine
minimizeHop m = translateFromHopcroft . H.hopcroft . translate2Hopcroft $ m

translate2Hopcroft :: Machine -> FS.DFA' Label
translate2Hopcroft m =
  FS.DFA' { FS.alpha = S.fromList $ L.map (\(x,(y,z)) -> y) $ transitions m
                               , FS.ss =  M.fromList $ lstate
                               , FS.accept = S.fromList $ L.map (\s -> read s :: Int) $ states m
                               , FS.st = read (tinit m) :: Int
                               }
  where mkStMap src = M.fromList $
                      L.map (\(x,(y,z)) -> (y, read z :: Int)) $
                      L.filter (\(x,(y,z)) -> x == src) $ transitions m
        lstate = L.map (\s -> (read s :: Int, mkStMap s)) $ states m

translateFromHopcroft :: FS.DFA' Label -> Machine
translateFromHopcroft dfa = Machine { states = nstates
                                    , tinit = show $ FS.st dfa
                                    , transitions = ntrans
                                    , accepts = nstates
                                    }
  where ntrans = concat $
                 L.map flatten $
                 L.map (\(x,y) -> (show x, M.toList y)) $ M.toList $ FS.ss dfa
        flatten (s, xs) = L.map (\(y,z) -> (s,(y, show z))) xs
        nstates = nub $ concat $ L.map (\(x,(y,z)) -> [x,z]) ntrans
