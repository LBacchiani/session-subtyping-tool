{-# LANGUAGE DeriveDataTypeable #-}

import Parser
import GenModel
import GenFormula
import MCRLBridge
import WitnessTree
import KozenEtAl (reduce, genTermAutomata, aproduct, kozenSubtype)

import GayAndHole (mkGHsubtyping)
-- import BLocalType (mksubtyping)

import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Tree
import System.Environment
import System.FilePath.Posix
import System.Process
import System.Console.CmdArgs
import GHC.IO.Handle
import Control.Monad
import Data.Text (strip, pack, unpack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Printf (printf)

-- DEBUG
import System.IO.Unsafe
import Debug.Trace

writeToFile :: FilePath -> String -> IO()
writeToFile file content = writeFile file content



data Interaction = Passive
                 | Interactive
                 deriving (Data,Typeable,Show,Eq)

data SubtypingMode = GenSub
                   | GenSup
                   | GenAsync
                   | GenAll
                   | TestAsync
                   | GayHole
                   | Kozen
                   | SizeInfo
                   deriving (Data,Typeable,Show,Eq)

data Subtyping = Subtyping
                 { interaction :: Interaction
                 , typingmode :: SubtypingMode
                 , sub :: String
                 , sup :: String
                 , pics :: Bool
                 }
               deriving (Data,Typeable,Show,Eq)
submodes =  enum
           [ GenSub &= help "Generate a formula from the subtype (synchronous)" &= name  "B"
           , GenSup &= help "Generate a formula from the supertype (synchronous)" &= name "P"
           , GenAll &= help "All of the above" &= name "X"
           , GayHole  &= help "Gay and Hole subtyping algorithm" &= name "G"
           , Kozen &= help "Kozen et al. subtyping algorithm" &= name "K"
           , SizeInfo  &= help "Print size info" &= name "S"
           ]


subargs = Subtyping
 { interaction = enum [ Passive &= help "Passive mode (arguments are paths to files)"
                      , Interactive &= help "Interactive mode (arguments are given in line)"
                      ]
 , typingmode = submodes
 , sub = def  &= argPos 0  &= typ "FILE/LOCALTYPE"
 , sup = def &= argPos 1  &= typ "FILE/LOCALTYPE"
 , pics = def
           &= explicit &= name "pics"
           &= help "Print graphs (.dot and .png files)"
 }  &= help "Session type subtyping relations as model checking problems"



getLocalTypeString :: Interaction -> String -> IO String
getLocalTypeString Passive s = readFile s
getLocalTypeString Interactive s = return s


main :: IO ()
main = do
  pargs <- cmdArgs (modes [subargs])
  subinp <- getLocalTypeString (interaction pargs) (sub pargs)
  supinp <- getLocalTypeString (interaction pargs) (sup pargs)
  case parseLocalType subinp of
    Left err -> do
                 putStrLn "Error in (T)ype"
                 print err
    Right subans ->
      case parseLocalType supinp of
        Left err -> do
                    putStrLn "Error in (S)uperype"
                    print err
        Right supans ->
          if not (wellFormed subans) then putStrLn "Error: (T)ype not well-formed."
          else if not (wellFormed supans) then putStrLn "Error: (S)upertype not well-formed."
          else
            do
              let printInfo t = do
                    let rec = length $ M.toList $ recursionDistance t M.empty
                    let (s,r) = numberOfTransitions t
                    putStr $ "Transitions: "++(show $ s+r )++" "++(show $ (s,r))++" | "
                    putStr $ "Depth: "++(show $ typeDepth t)++" | "
                    putStr $ "Recursions: "++(show $ numberOfRecursions t)++" | "
                    putStr $ "1st recursion: "++(show $ firstRecursion t)++" | "
                    putStr $ "Max recursion dist: "++(show $ maxRecursion t)++" | "
                    putStr $ "Rec leaves: "++(show $ rec)++" | "
                    putStr $ "Branching Factor: "++(printf "%.2f" (branchingFactor t))++" | "
                    putStrLn $ "Leaves: "++(show $ numberOfLeaves t)
              let printMeasures t = do
                    let rec = length $ M.toList $ recursionDistance t M.empty
                    let (s,r) = numberOfTransitions t
                    putStrLn $ (show $ s+r) -- Transitions / Messages: **num(T)**
                    putStrLn (show $ (s)) -- Send messages
                    putStrLn (show $ (r)) -- Receive messages
                    putStrLn $ (show $ (typeDepth t)) -- Depth
                    putStrLn $ show $ sizeUnfolded [] t  -- **unf(T)**
                    putStrLn $ (printf "%.2f" (branchingFactor t)) -- branching factor
                    putStrLn $ (show $ numberOfLeaves t) -- number of leaves

              when ((typingmode pargs) == SizeInfo) $
                do -- putStr "Subtype: "
                  printMeasures subans
                  -- putStr "Suptype: "
                  printMeasures supans
              let (subsnds,subrcvs) = allActions subans
              let suballactions = subsnds++subrcvs
              --
              let (supsnds,suprcvs) = allActions supans
              let supallactions = supsnds++suprcvs
              --
              let allsends = L.nub $ subsnds++supsnds
              let allreceives = L.nub $ subrcvs++suprcvs
              let allactions = allsends++allreceives
              --

              when (((typingmode pargs) == GenSup) || ((typingmode pargs) == GenAll)) $
                do
                  -- putStr $ "[SYNC-SUP]  model(sub) |= sup(SUP):  "
                  writeToFile "sub.crl2" (genModel allsends allreceives subans)
                  let supformula =  subtype Receive allactions supans
                  writeToFile "sup.mcf" $ printStateFormula supformula
                  start <- getPOSIXTime
                  parseModelChecker "sub.crl2" "sup.mcf" >>= (putStrLn . show)
                  --
                  end <- getPOSIXTime
                  putStrLn $ (show $ end - start)
              when (((typingmode pargs) == GenSub) || ((typingmode pargs) == GenAll)) $
                do
                  -- putStr $ "[SYNC-SUB]  model(SUP) |= sub(sub):  "
                  writeToFile "sup.crl2" (genModel allsends allreceives supans)
                  let subformula = subtype Send allactions subans
                  writeToFile "sub.mcf" $ printStateFormula subformula
                  start <- getPOSIXTime
                  parseModelChecker "sup.crl2" "sub.mcf" >>= (putStrLn . show)
                  --
                  end <- getPOSIXTime
                  putStrLn $ (show $ end - start)
              -- SUB
              when ((typingmode pargs) == GayHole || ((typingmode pargs) == GenAll)) $
                do
                  start <- getPOSIXTime
                  let out = (mkGHsubtyping subans supans)
                  when (out) $
                    do
                    when ((pics pargs)) $
                     do
                      checkingAlgorithm (pics pargs) False 30 subans supans
                  putStrLn $ "Result: " ++ show out
                  end <- getPOSIXTime
                  putStrLn $ "Time: " ++ (show $ end - start)
              when ((typingmode pargs) == Kozen || ((typingmode pargs) == GenAll)) $
                do
                  let a1 = genTermAutomata . reduce $ subans
                  let a2 = genTermAutomata . reduce $ supans
                  start <- getPOSIXTime
                  --
                  putStr $ "Result: "
                  putStrLn $ show $ kozenSubtype $ aproduct a1 a2
                  --
                  end <- getPOSIXTime
                  when ((pics pargs)) $
                    putStrLn $ "Warning: kozen mode does not generate any simulations"
                  putStrLn  $ "Time: " ++ (show $ end - start)
              return ()



--  "mcrl22lps -v -lstack" seems to be (much) slower
parseModelChecker :: String -> String -> IO Bool
parseModelChecker model formula =
  let mcmd = "mcrl22lps -q "++model++" | lps2pbes -m -qf "++formula++" | pbes2bool -q"
  in do
    out <- readProcess "bash" ["-c",mcmd] []
    return $ (unpack. strip . pack $ out)=="true"
