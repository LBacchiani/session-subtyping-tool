module KozenEtAl where

import Data.Set as S
import Data.List as L
import Data.Map as M
import qualified Parser as P


-- DEBUG
import System.IO.Unsafe
import Debug.Trace


type Message = String


data Constructor = End
                 | Send [Message]
                 | Receive [Message]
                 deriving (Show, Eq, Ord)


preequal :: Constructor -> Constructor -> Bool
preequal End End = True
preequal (Send m) (Send m') = True
preequal (Receive m) (Receive m') = True
preequal _ _ = False

data State = State
             { sid :: String
             , label :: Constructor
             } deriving (Show, Eq, Ord)

type Edge = (State, (Message, State))

data TermAutomata = TermAutomata
                    { states :: [State]
                    , tinit :: State
                    , edges :: [Edge]
                    } deriving (Show, Eq, Ord)


type ProdState = (State, State)

type ProdEdge = (ProdState, (Message, ProdState))

data ProdAutomata = ProdAutomata
                    { pstates :: [ProdState]
                    , pinit :: ProdState
                    , pedges :: [ProdEdge]
                    , finals :: [ProdState]
                    } deriving (Show, Eq, Ord)

msgs :: TermAutomata -> State -> [Message]
msgs a s = L.map (fst . snd) $ L.filter (\(x,(y,z)) -> x == s) (edges a)


tsucc :: TermAutomata -> State -> Message -> State
tsucc a s m = head $ L.map (snd . snd) $ L.filter (\(x,(y,z)) -> (x == s) && (m==y)) (edges a)


subtype :: Constructor -> Constructor -> Bool
subtype End End = True
subtype (Send xs) (Send ys) = (S.fromList xs) `S.isSubsetOf` (S.fromList ys)
subtype (Receive xs) (Receive ys) = (S.fromList ys) `S.isSubsetOf` (S.fromList xs)
subtype t s = False

nextState :: TermAutomata -> State -> TermAutomata -> State -> [ProdEdge]
nextState a1 s1 a2 s2 =
  let msg1 = msgs a1 s1
      msg2 = msgs a2 s2
      inter = L.intersect msg1 msg2
  in L.map (\x -> ((s1,s2), (x,(tsucc a1 s1 x, tsucc a2 s2 x)))) inter

aproduct :: TermAutomata -> TermAutomata -> ProdAutomata
aproduct a1@(TermAutomata s1 i1 e1) a2@(TermAutomata s2 i2 e2) =
  let nstates = [(x,y) | x <- s1, y <- s2]
      ninit = (tinit a1, tinit a2)
      nedges =  L.foldr (++) [] $ L.map (\(p,q) -> nextState a1 p a2 q) (nstates)
      nfinals = L.filter (\(x,y) -> not $ subtype (label x) (label y)) nstates
  in ProdAutomata { pstates = nstates
                  , pinit = ninit
                  , pedges = nedges
                  , finals = nfinals
                  }

genState :: String -> (Map String State) -> P.LocalType -> State
genState s map (P.Rec s' t) = genState s map t
genState s map (P.Choice dir xs) = let c = if dir == P.Send
                                           then Send (L.map fst $ P.getPairs xs)
                                           else Receive (L.map fst $ P.getPairs xs)
                                   in State {sid = s, label = c}
genState s map (P.Act dir s' t) = genState s map (P.Choice dir [(P.Act dir s' t)])
genState s map (P.End) = State {sid = s, label = End}
genState s map (P.Var x) = case M.lookup x map of
  Just y -> y
  Nothing -> error $ "ill formed type"



genTermAutomata :: P.LocalType -> TermAutomata
genTermAutomata t = let nedges = L.nub $ genEdges ninit M.empty [] t
                        nstates =  L.nub $ ninit:(L.foldr (++) [] $ L.map (\(s,(m,t)) -> [s,t]) nedges)
                        ninit = genState "root" M.empty t
                    in TermAutomata
                       { states = nstates
                       , tinit = ninit
                       , edges = nedges
                       }
  where genEdges prev map acc (P.Rec s t) = genEdges prev (M.insert s prev map) acc t
        genEdges prev map acc (P.Act dir s t) =
          let next = genState ((sid prev)++s) map t
          in (prev, (s, next)):(genEdges next map acc t)
        genEdges prev map acc (P.Choice dir xs) = L.foldr (++) [] (L.map (genEdges prev map acc) xs)
        genEdges prev map acc (P.End) = []
        genEdges prev map acc (P.Var x) = []

reduce :: P.LocalType -> P.LocalType
reduce t = helper Nothing t
  where helper b (P.Rec s t) = case b of
          Just x -> helper b (P.substitute s (P.Var x) t)
          Nothing -> P.Rec s (helper (Just s) t)
        helper b (P.Choice dir xs) = P.Choice dir (L.map (helper b ) xs)
        helper b (P.Act dir s t) = P.Act dir s (helper Nothing t)
        helper b t = t



kozenSubtype :: ProdAutomata -> Bool
kozenSubtype (ProdAutomata sts init es fs) = not $ helper [] [init]
  where helper visited [] = False
        helper visited (src:xs)
          | src `L.elem` visited = helper visited xs
          | otherwise = (src `L.elem` fs)
                        ||
                        (
                          let nexts = L.map (snd . snd)
                                      $ L.filter (\(x,(y,z)) -> x == src) es
                          in helper (src:visited) (xs++nexts)
                        )
