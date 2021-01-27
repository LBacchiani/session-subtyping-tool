{-# LANGUAGE DeriveDataTypeable #-}

import Parser

-- import GayAndHole (mkGHsubtyping)
-- import BLZsubtyping (mkLBZsubtyping)
-- import BLocalType (mksubtyping)

import Algo (checkingAlgorithm)

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

data SubtypingMode = BLZ
                   deriving (Data,Typeable,Show,Eq)

data Subtyping = Subtyping
                 { interaction :: Interaction
                 , typingmode :: SubtypingMode
                 , sub :: String
                 , sup :: String
                 , debug :: Bool
                 , bound :: Int
                 , dualflag :: Bool
                 }
               deriving (Data,Typeable,Show,Eq)
submodes =  enum
           [ BLZ  &= help "BLZ subtyping relation (default, no need to specify)" &= name "G"
           ]


subargs = Subtyping
 { interaction = enum [ Passive &= help "Passive mode (arguments are paths to files)"
                      , Interactive &= help "Interactive mode (arguments are given in line)"
                      ]
 , typingmode = submodes
 , sub = def  &= argPos 0  &= typ "FILE/LOCALTYPE"
 , sup = def &= argPos 1  &= typ "FILE/LOCALTYPE"
 , bound = def &= opt "-1"  &= argPos 2  &= typ "INT"
 , debug = def
           &= explicit &= name "debug"
           &= help "Print debug info and pictures (png)"
 , dualflag = def
              &= explicit &= name "dual"
              &= help "tests for dual(m1) > dual(m2)"
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
              let b = if ((bound pargs) == -1)
                      then 2*(typeDepth supans)
                      else bound pargs
              start <- getPOSIXTime
              checkingAlgorithm b (dualflag pargs) (debug pargs) False subans supans
              end <- getPOSIXTime
              putStrLn $ ("Time: " ++ (show $ end - start))
            --  return ()
