{-# LANGUAGE DeriveGeneric #-}


module Algo where

import Parser
import Automata

import Control.Monad
import System.Process

import Data.List as L
import Data.Set as S
import Data.Map as M
import Data.Tuple (swap)
import Data.Either (lefts)
import Data.Maybe
import GHC.Generics (Generic)
import Data.Maybe

-- DEBUG
import System.IO.Unsafe
import Debug.Trace


type Value = (State, Machine)
type IValue = (String, Value)
type PrintV = (NType, IValue)
type Ancestors = Map IValue IValue
type Edge = (PrintV, (Label, PrintV))
type NodeMap = Map IValue String
data CTree = Node
             { label :: IValue
             , children :: [(Label, CTree)]
             , status :: NType
             }
           deriving (Show, Eq)


data CtxtA = JHole Int | KHole Int | CtxtA [(Label, CtxtA)]
           deriving (Show, Eq)

data NType = Increase | Decrease |
             Seen | Interm | Bound | Removable |
             Tmp | Keep
           deriving (Show, Eq, Ord)

checkingAlgorithm :: Int -> Bool -> Bool -> Bool -> LocalType -> LocalType -> IO ()
checkingAlgorithm bound dual debug nomin t1 t2 =
  let fm = if dual then dualMachine else id
      (m1, m2) = (if dual then swap else id) $ (fm $type2Machine nomin "-" t1, fm $ type2Machine nomin "+" t2)
  in do case buildTree bound debug m1 m2 of
          Nothing -> putStrLn "Result: Maybe"
          Just (b',(to,ancs), errors) ->
            let t = tagRemovable m1 to
                b = (not $ isControllable m2) || b'
            in
            do case prune m1 ancs t of
                 Nothing -> do putStrLn ("Result: " ++ (if not(b) && errors == [] then "Maybe" else show b))
                               when debug $
                                 do
                                    printDebugInfo m1 m2 t [] ancs errors
                                    when(not $ isControllable m2) $ putStrLn "Uncontrollable supertype: empty simulation graph generated"

                 Just t' -> do let ts = splitTree ancs t'
                               putStrLn ("Result: " ++ (if (not(b) && errors == []) || (b && not((L.all (goodTree bound m1 ancs) ts))) then "Maybe" else show b))
                               when debug $
                                 do
                                    printDebugInfo m1 m2 t ts ancs errors
                                    when(not $ isControllable m2) $ putStrLn "Uncontrollable supertype: empty simulation graph generated"

subCheck :: Int -> Machine -> Machine -> Bool
subCheck bound m1 m2 =
  case buildTree bound False m1 m2 of
    Nothing -> False
    Just (b',(to,ancs), _) ->
      let t = tagRemovable m1 to
          b = (not $ isControllable m2) || b'
      in case prune m1 ancs t of
          Nothing -> b
          Just t' -> let ts = splitTree ancs t'
                     in b && (L.all (goodTree bound m1 ancs) ts)


printDebugInfo :: Machine -> Machine -> CTree -> [CTree] -> Ancestors -> [IValue] -> IO ()
printDebugInfo m1 m2 t ts ancs errors =
  do
    writeToFile "tmp/simulation_tree.dot" (printTrees ancs [t] errors)
    writeToFile "tmp/witness_trees.dot" (printTrees ancs ts errors)

buildTree :: Int -> Bool -> Machine -> Machine -> Maybe (Bool, (CTree, Ancestors), [IValue]) -- The [IValue] represents all the errors found in the simulation game
buildTree bound debug m1 m2 = helper bound ("0", (tinit m1, m2)) []

  where helper :: Int -> IValue -> [(Label, IValue)] -> Maybe (Bool, (CTree, Ancestors), [IValue])
        helper b v seen
          | b == 0 =  Just (False, ((Node v [] Interm), M.empty), [])
          | otherwise = case findTwo v (L.map snd seen) of
            Just anc -> Just (True, ((Node v [] Increase), M.singleton v anc), [])
            Nothing -> case oneStep debug m1 (snd v) of
                Just ys -> continuation b v (nextconfs ys (fst v)) seen
                Nothing -> Just (False, ((Node v [] Interm), M.empty), [v])

        nextconfs :: [(Label, Value)] -> String -> [(Label, IValue)]
        nextconfs xs i = snd $ mapAccumL (\x y -> (x+1, (fst y, (i++(show x), snd y )))) 0 xs

        continuation :: Int -> IValue -> [(Label, IValue)] -> [(Label, IValue)] -> Maybe (Bool, (CTree, Ancestors), [IValue])
        continuation b v xs seen = case sequencePair $ L.map (\(a,x) -> (a, helper (b-1) x ((a,v):seen))) xs of
          Nothing -> Nothing
          Just ys -> let zs = L.map (\(l,(b,(t,ancs),err)) -> (l,t)) ys
                         mps = L.map (\(l,(b,(t,ancs),err)) -> ancs) ys
                         res = and $ L.map (\(l,(b,(t,ancs),err)) -> b) ys
                         errors = L.concat $ L.map (\(l,(b,(t,ancs),err)) -> err) ys
                     in Just (res, ((Node v zs Interm), M.unions mps), errors)




splitTree :: Ancestors -> CTree -> [CTree]
splitTree ancs t = helper realancs t
  where helper acs (Node v xs f)
          | v `L.elem` acs = [Node v xs f]
          | otherwise = concat $ L.map (\(x,c) -> helper acs c) xs
        realancs = L.map snd $ L.filter (\(x,y) -> x `L.elem` allnodes) $ M.toList ancs
        gnodes (Node v xs f) = (v:(concat $ L.map (gnodes . snd) xs))
        allnodes = gnodes t

goodTree :: Int -> Machine -> Ancestors -> CTree -> Bool
goodTree bound m1 ancs t@(Node v xs f) = L.all checkLeaf $ leaves t
  where descendants =  L.map fst $ L.filter (\((i,(s,m)),(j,(s',m'))) -> (not $ bisimilar m m')) $ M.toList ancs
        leaves (Node v [] f) = [v]
        leaves (Node v xs f) = concat $ L.map (leaves . snd) xs
        nodemachines (Node (s,(q,m)) xs f) = m:(concat $ L.map (nodemachines . snd) xs)
        ctxt = maximum $ catMaybes $ L.map extractA (nodemachines t)
        checkLeaf n@(s,(q,m)) = case M.lookup n ancs of
          Nothing -> (isFinalConf m1 (q,m))
          Just a -> checkRel n a
        checkRel (s,(q,m)) (s',(q',m')) =
          (q == q')
          &&
          (
            (isFinalConf m1 (q,m))
            ||
            (bisimilar m m')
            ||
            (related ctxt m' m)
            ||
            ( (related ctxt m m')
              &&
              (subCheck bound (cleanUp $ updateInit q m1) m)
            )
            ||
            (
              let (km,jm) = holesOfMachine ctxt m
              in ((M.size jm) == 0)
                 &&
                 (subCheck bound (cleanUp $ updateInit q m1) m)
            )
          )

related :: CtxtA -> Machine -> Machine -> Bool
related ca m1 m2 = (checkMap jm1 jm2) && (checkMap km1 km2)
  where (km1, jm1) = holesOfMachine ca m1
        (km2, jm2) = holesOfMachine (nestA ca) m2
        checkMap n1 n2 = L.and [ bisimilar v1 v2 |
                                 (k1,v1) <- M.toList n1,
                                 (k2,v2) <- M.toList n2,
                                 k1==k2 ]


nestA :: CtxtA -> CtxtA
nestA c = helper c
  where helper (KHole i) = KHole i
        helper (JHole i) = c
        helper (CtxtA xs) = CtxtA $ L.map (\x -> (fst x, helper . snd $ x )) xs

holesOfMachine :: CtxtA -> Machine -> (Map Int Machine, Map Int Machine)
holesOfMachine ca m1 = (kholes ca m1, jholes ca m1)
  where kholes (KHole i) m = M.singleton i m
        kholes (JHole i) m = M.empty
        kholes (CtxtA xs) m =
          let mmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==(tinit m)) $ transitions m
              next = [(cb, cleanUp $ updateInit y m) |
                      (a,cb) <- xs,
                      (b,y) <- mmoves,
                      b==a]
          in M.unions $ L.map (\x -> kholes (fst x) (snd x)) next

        jholes (JHole i) m = M.singleton i m
        jholes (KHole i) m = M.empty
        jholes (CtxtA xs) m =
          let mmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==(tinit m)) $ transitions m
              next = [(cb, cleanUp $ updateInit y m) |
                      (a,cb) <- xs,
                      (b,y) <- mmoves,
                      b==a]
          in M.unions $ L.map (\x -> jholes (fst x) (snd x)) next



depthCtxtA :: CtxtA -> Int
depthCtxtA (JHole _) = 1
depthCtxtA (KHole _) = 1
depthCtxtA (CtxtA xs) = maximum $ L.map (depthCtxtA . snd) xs

compareCtxtA :: CtxtA -> CtxtA -> Ordering
compareCtxtA c1 c2 =
  let dp = compare (depthCtxtA c1) (depthCtxtA c2)
  in if dp /= EQ
     then dp
     else helper c1 c2
       where helper (JHole _) _ = LT
             helper (KHole _) _ = LT
             helper (CtxtA xs) (CtxtA ys) = compare (length xs) (length ys)

instance Ord CtxtA where
  compare = compareCtxtA


sequencePair :: [(a, Maybe b)] -> Maybe [(a,b)]
sequencePair [] = Just []
sequencePair ((x,Just y):xs) = case sequencePair xs of
  Just ys -> Just ((x,y):ys)
  Nothing -> Nothing
sequencePair ((x,Nothing):xs) = Nothing

equalConf :: Bool -> IValue -> IValue -> Bool
equalConf debug (i1, (p,m)) (i2, (p',m')) =
  (p==p') && (equivRepeat debug m' m)

tagRemovable :: Machine -> CTree -> CTree
tagRemovable m1 t = helper [] t
  where helper seen n@(Node (i,v) xs f)
          | isFinalConf m1 v = tag n
          | L.any (nodeeq v) seen = tag n
          | L.null xs = n
          | otherwise =
            let ys = L.map (\(a,x) -> (a, helper (v:seen) x)) xs
                rm = L.all (\(a,(Node _ _ tg)) -> tg == Removable) ys
            in if rm
               then (Node (i,v) ys Removable)
               else (Node (i,v) ys Keep)
        tag (Node (i,v) xs _) = (Node (i,v) xs Removable)
        nodeeq (p,m) (p',m') = (p==p') && (bisimilar m m')


prune :: Machine -> Ancestors -> CTree -> Maybe CTree
prune m1 ancs t = remove t  -- $ (tagRemovable m1 t)
  where remove (Node v xs Removable) = Nothing
        remove (Node v xs Keep) =
          if v `L.elem` realancs
          then Just (Node v xs Keep) -- if you're an ancestor but
               -- unmarked, dont prune below!
          else let ys = catMaybes $ L.map (\(a,x) -> case remove x of
                                              Just y -> Just (a,y)
                                              Nothing -> Nothing
                                          ) xs
               in Just (Node v ys Keep)
        remove n = Just n
        realancs = L.map snd $ L.filter (\(v,v') -> not $ ivBisim v v') $ M.toList ancs

ivBisim :: IValue -> IValue -> Bool
ivBisim (i,(p,m)) (j,(p',m')) = (p==p') && (bisimilar m m')


findTwo :: IValue -> [IValue] -> Maybe IValue
findTwo v seen =
  case L.filter (equalConf False v) seen of
    [] -> Nothing
    [x] -> if (ivBisim v v) then (Just x) else Nothing
    (x:xs) -> Just x




equivRepeat :: Bool -> Machine -> Machine -> Bool
equivRepeat debug m1 m2 = (helper (tinit m1, tinit m2) [])
  where helper (p,q) seen
          | (p,q) `L.elem` seen = True
          | otherwise =
            (bisimilar (updateInit p m1) (updateInit q m2))
            ||
            (
              (nonempty p)
              &&
              ((ssucc . removess $ p) == (removess q))
              &&
              (
                let pmoves = successors m1 p
                    qmoves = successors m2 q
                in ((S.fromList (L.map fst pmoves)) == (S.fromList (L.map fst qmoves)))
                   && helper (p,unpeel q) ((p,q):seen)
              )
            )
        removess xs = L.filter ('s'/=) xs

ssucc :: State -> State
ssucc s = "c"++s

nonempty :: State -> Bool
nonempty ('c':xs) = True
nonempty _ = False

unpeel :: State -> State
unpeel ('c':x:xs)
  | (x /= 'c') = 's':x:xs
  | otherwise = 'c':(unpeel (x:xs))
unpeel ('s':xs) = 's':xs
unpeel [] = []

osucc :: State -> State
osucc s = "s"++s


clevel :: State -> Int
clevel xs = L.length $ L.filter ((==)'c') xs

slevel :: State -> Int
slevel xs = L.length $ L.filter ((==)'s') xs

bornid :: State -> Int
bornid xs = read (L.filter (\x -> x/='s' && x/='c') xs) :: Int


extractA :: Machine -> Maybe CtxtA
extractA m = helper (tinit m)
  where helper q
          | (clevel q) > 1 = let k = clevel q
                             in Just $ CtxtA $ L.map (\(x,y) -> (x, build y k)) $ successors m q
          | otherwise = Nothing
        build q k
          | (clevel q) == k = CtxtA $ L.map (\(x,y) -> (x, build y k)) $ successors m q
          | (clevel q) < k =
            if (slevel q) == k
            then KHole (bornid q)
            else JHole (bornid q)

isFinalConf :: Machine -> Value -> Bool
isFinalConf m1 (p, m) = (isFinal m1 p) && (isFinal m (tinit m))


-- extractA :: Machine -> Maybe (CtxtA, [Int], [Int])
-- extractA m = helper (tinit m) []
--   where helper q seen
--           | q `L.elem` seen = Nothing
--           | nonempty q =
--             let qmoves = successors m q
--           | otherwise = Nothing


inControllableBarb :: Machine -> State -> Set Message
inControllableBarb m p =
  S.fromList $
  L.map (snd . fst . snd) $
  L.filter (\(s,((d, lab),t)) ->
             s==p && d==Receive && (isControllable $ updateInit t m) ) $ transitions m

oneStep :: Bool -> Machine -> Value -> Maybe [(Label, Value)]
oneStep debug m1 v@(p,m)
  | isFinalConf m1 v = --(if debug then (trace ("Final: "++(show (p,(tinit m)))++"\n"++(printMachine m)) ) else (\x  -> x  )) $
    Just []
    --
  | not $ isControllable m = Just []
    --
  | (isInput m1 p) && (isInput m (tinit m)) && ((inControllableBarb m (tinit m)) `isSubsetOf` (inBarb m1 p)) =
      --(if debug then (trace ("In: "++(show (p,(tinit m)))) ) else (\x  -> x  )) $
      let  psmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==p) $ transitions m1
           qsmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==(tinit m)) $ transitions m
           next = L.nub
                  $ [(a,(x, cleanUp $ updateInit y m)) |
                     (a,x) <- psmoves,
                     (b,y) <- qsmoves,
                     c <- S.toList (inControllableBarb m (tinit m)),
                     b==(Receive, c),
                     a==b]
      in Just next
         --
  | (isOutput m1 p) && (isOutput m (tinit m)) && ((outBarb m1 p) == (outBarb m (tinit m))) =
        --(if debug then (trace ("OutSync: "++(show (p,(tinit m)))++"\n"++(printMachine m)) ) else (\x  -> x  )) $
        let psmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==p) $ transitions m1
            qsmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==(tinit m)) $ transitions m
            next = L.nub $ [(b,(x,cleanUp $ updateInit y m)) | (a,x) <- psmoves, (b,y) <- qsmoves, a==b]
        in Just next
  | (isOutput m1 p) && not (isOutput m (tinit m)) =
          --(if debug then (trace ("Out: "++(show (p,(tinit m)))++"\n"++(printMachine m)) ) else (\x  -> x  )) $
          let psmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==p) $ transitions m1
              qstates = reachableSendStates (tinit m) m
              newmachines = L.map (\a -> ((Send, a), replaceInMachine m (Send, a) qstates)) $ S.toList (outBarb m1 p)
              next = L.nub $ [(a, (x, updateInit (ssucc (tinit m)) m'))| (a,x) <- psmoves, (b,m') <- newmachines, a==b]
          in if (not $ L.null qstates)
                &&
                (L.and $ L.map (\x -> (outBarb m1 p) == (outBarb m x)) qstates)
             then Just next
             else --(if debug then (trace ("BadOut: "++(show (p,(tinit m)))++"\n"++(printMachine m)) ) else (\x  -> x  ))
                  Nothing
  | otherwise = --(if debug then (trace ("Bad: "++(show (p,(tinit m)))++"\n"++(printMachine m)) ) else (\x  -> x  )) $
                Nothing




replaceInMachine :: Machine -> Label -> [State] -> Machine
replaceInMachine m l qs = cleanUp $ Machine { states = nstates
                                            , tinit = ssucc $ tinit m
                                            , transitions = nedges
                                            , accepts = nstates
                                            }
  where alltrans = nub $ transitions m
        nstates = nub $ concat $ L.map (\(s,(a,t)) -> [s,t]) nedges
        nedges = (normalisedEdges prefix) `L.union` (normalisedEdges suffix)
        prefix = (setRight (alltrans L.\\ filtered)) `L.union` replaced
        setRight xs = L.map (\(s,(a,t)) -> (Right s,(a, Right t))) xs
        suffix = L.map (\(s,(a,t)) -> (Left s,(a,Left t))) alltrans
        normalise (Right s) = ssucc s
        normalise (Left s) = osucc s
        normalisedEdges xs =  L.map (\(s,(a,t)) -> (normalise s,(a, normalise t))) xs
        replaced = [(Right s1, (l1, Left t2)) | (s1,(l1,t1)) <- alltrans
                                              , (s2,(l2,t2)) <- alltrans
                                              , t1==s2
                                              , s2 `elem` qs
                                              , l2 == l
                                              , isReceiveLab l1
                                              ]
        filtered = [(s1,(l1,t1)) | (s1,(l1,t1)) <- alltrans , t1 `elem` qs]





-- --------------------- PRINTING STUFF -----------------


mkNodeMap :: Int -> [PrintV] -> NodeMap
mkNodeMap i xs = M.fromList $ L.map (\(tag, (id,y)) -> ((id,y),"node"++(show i)++id)) xs

printNodeId :: NodeMap -> PrintV -> String
printNodeId  map (tag,v) = case M.lookup v map of
  Just s -> s
  Nothing -> error $ "Node "++(show v)++" not found."

printNode :: NodeMap -> [IValue] -> PrintV -> String
printNode map errors v@(tag, (i,(p,m))) =
  let
     nm = rename (\s -> i++s) m
     node = (printNodeId map v)
  in "subgraph cluster_conf"++node++" {\n"
     ++(if node == "node00" then "graph[penwidth=3]\n" else "")
     ++(if (i,(p,m)) `L.elem` errors then "graph[color=\"red\"]\n" else "")
     ++"label = \" L"++i++": "++p++"("++(printTag tag)++")"++"\";\n"
     ++(printNodeId map v)++"[style = invis];\n" -- style = invis
     ++(printMachineBody nm)
     ++"}"

mkGraph :: CTree -> (PrintV, [Edge])
mkGraph (Node v xs tag) = ((tag, v), helper (tag, v) xs)
  where helper n ys =
          concat $
          L.map (\(l, (Node w zs tag')) ->
                  (n,(l,(tag', w))):(helper (tag', w) zs)) ys


printEdge :: NodeMap -> Edge -> String
printEdge map (s,(l,t)) = (printNodeId map s)++" -> "++(printNodeId map t)
                          ++"[style=filled,lhead=cluster_conf"++(printNodeId map t)
                          ++", ltail=cluster_conf"++(printNodeId map s)
                          ++", label=\""++(printEdgeLabel l)++"\", minlen=3];"

printAncestorEdge :: NodeMap -> (IValue, IValue) -> String
printAncestorEdge map (s',t') =
  let s = (Tmp, s')
      t = (Tmp, t')
  in  (printNodeId map s)++" -> "++(printNodeId map t)
      ++" [style=filled,color=blue,lhead=cluster_conf"++(printNodeId map t)
      ++", ltail=cluster_conf"++(printNodeId map s)
      ++", minlen=3, style=dashed, color=black];"



printGraph :: Int -> PrintV -> [Edge] -> Ancestors -> [IValue] -> String
printGraph i init edges ancestors errors =
  let nodes = nub $ concat $ L.map (\(s,(l,t)) -> [s,t]) edges
      nmap = mkNodeMap i nodes
      snodes = intercalate "\n" $ L.map (printNode nmap errors ) nodes
      sedges = intercalate "\n" $ L.map (printEdge nmap) edges
      ancs = intercalate "\n" $ L.map (printAncestorEdge nmap) $
             L.filter (\(t,s) -> (inlist t) && (inlist s)) $
             M.toList ancestors
      inlist t = t `L.elem` (L.map snd nodes)
  in snodes++"\n"
     ++sedges++"\n"
     ++ancs++"\n"

printTree :: Int -> Ancestors -> CTree -> [IValue] -> String
printTree i ancs t errors = let (init, edges) = mkGraph t
                            in printGraph i init edges ancs errors

printTrees :: Ancestors -> [CTree] -> [IValue] -> String
printTrees ancs list errors =  "digraph CTs { \n graph [fontsize=10 fontname=\"Verdana\" compound=true]; \n node [shape=record fontsize=10 fontname=\"Verdana\"]; \n rankdir = TD ranksep = 1.2 nodesep = 0.5; \n"++(helper $ snd $ mapAccumL (\x y -> (x+1, (x,y))) 0 list)++"}\n"
  where helper ((i,x):xs) =
          let header = "subgraph cluster_"++(show i)
                       ++" {\n"
              --         ++ "label = \""++(show i)++"\";\n"
              footer = "}\n"
              tr = printTree i ancs x errors
          in (header++tr++footer)++"\n"++(helper xs)
        helper [] = ""



printEdgeLabel :: Label -> String
printEdgeLabel (Send, msg) = "!"++(msg)
printEdgeLabel (Receive, msg) = "?"++(msg)

printTag Removable = "R"
printTag Keep = "K"
printTag _ = "U"
