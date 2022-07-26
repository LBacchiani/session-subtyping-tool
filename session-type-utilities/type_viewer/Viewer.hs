{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}

import Automata
import Parser
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
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Text.Printf (printf)

-- DEBUG
import System.IO.Unsafe
import Debug.Trace

writeToFile :: FilePath -> String -> IO()
writeToFile file content = writeFile file content


data Subtyping = Subtyping
                 {t :: String}
               deriving (Data,Typeable,Show,Eq)


subargs = Subtyping
 {t = def  &= argPos 0  &= typ "FILE/LOCALTYPE"}  &= help "Type visualiser"



getLocalTypeString :: String -> IO String
getLocalTypeString s = readFile s


main :: IO ()
main = do
  pargs <- cmdArgs (modes [subargs])
  ty <- getLocalTypeString (t pargs)
  case parseLocalType ty of
    Left err -> print err
    Right ans ->
          if not (wellFormed ans)
          then putStrLn "Error in local type (not well-formed)."
          else
            do
             let m1 = type2Machine True "-" ans
             machine2file m1 "type"
             putStrLn "Done"
  return ()
