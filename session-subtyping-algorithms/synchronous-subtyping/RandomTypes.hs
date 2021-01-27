-- module RandomTypes where

-- This is an ad-hoc tool to generate random session types
-- will be made user-enabled soon.

import Data.List (nub)

import Control.Applicative ((<$>))
import Control.Monad (liftM, liftM2, replicateM)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck
import System.Random (getStdGen, StdGen)
import Control.Monad.Zip (mzip)

import Data.Time (UTCTime, getCurrentTime)
import Data.String.Utils (replace) -- cabal install MissingH
import Data.Char (toLower)

-- DEBUG
import System.IO.Unsafe
import Debug.Trace

data Variable = Var String
              deriving (Show, Eq)
                       
data Message = Msg String
             deriving (Show, Eq)
                      
data Direction = Send | Receive
               deriving (Show, Eq)

data SessionType = End
                 | Choice Direction [(Message, SessionType)]
                 | RecDef Variable SessionType
                 | RecCall Variable
                 deriving (Eq)

instance Show SessionType where
  show = printSessionType   

wellFormed :: SessionType -> Bool
wellFormed lt = helper [] lt
  where helper vs End = True
        helper vs (RecDef (Var s) lt) = if s `elem` vs 
                                  then False
                                  else helper (s:vs) lt
        helper vs (RecCall (Var s)) = s `elem` vs
        helper vs (Choice dir []) = False
        helper vs (Choice dir l@(x:xs)) = 
          let msgs = map fst l 
          in if (length msgs) == (length $ nub msgs)
             then and $ map (helper vs . snd) l
             else False

disjoint :: [Message] -> Bool
disjoint msgs = (length msgs) == (length $ nub msgs)
                   
                   
instance Arbitrary Variable where
  arbitrary = elements $ map (\x -> Var [x]) ['A'..'Z']
         
  
instance Arbitrary Message where
  arbitrary = sized list
    where list n = elements $ map (\x -> Msg [x]) (take 7 ['a'..'z'])

instance Arbitrary Direction where
  arbitrary = elements [Send, Receive]

instance Arbitrary SessionType where
  arbitrary = sized (sessterm [] False)
   
sessterm :: [String] -> Bool -> Int -> Gen SessionType
sessterm vars flag 0 = if flag 
                       then elements [End]
                       else elements (End:(map (\x -> RecCall (Var x)) vars))
sessterm vars flag n = 
  do let available = filter (\x -> not $ [x] `elem` vars) (take (maximum [n `div` 2, 1]) ['A'..'Z'])
     nvar <- elements available
     msgs <- fmap nub $ listOf1 arbitrary :: Gen [Message]
     rec <- elements vars
     recdef <- sessterm ([nvar]:vars) True n 
     dir <- arbitrary
     nexts <- vectorOf (length msgs) (sessterm vars False (n-1))  :: Gen [SessionType]
     let pairs = zip msgs nexts
     elements [ if n < 2 
                then End
                else Choice dir pairs
              -- , if (null vars || flag)
              --   then Choice dir pairs
              --   else RecCall (Var rec)
              -- , if null available
              --   then End
              --   else RecDef (Var [nvar]) recdef
              , Choice dir pairs
              ]
       
recalterterm :: [String] -> Bool -> Int -> Gen SessionType
recalterterm vars flag n 
  | n > 0 = do
    let available = filter (\x -> not $ [x] `elem` vars) (take (maximum [n `div` 2, 1]) ['A'..'Z'])
    nvar <- elements available
    msgs <- fmap nub $ listOf1 arbitrary :: Gen [Message]
    nexts <- vectorOf (length msgs) (recalterterm vars False (n-1))  :: Gen [SessionType]
    dir <- arbitrary
    recdef <- recalterterm ([nvar]:vars) True n 
    let pairs = zip msgs nexts
    if flag || null available
      then elements [ Choice dir pairs ]
      else elements [ RecDef (Var [nvar]) recdef ]
  | otherwise = do
    rec <- elements vars
    if flag 
    then elements [ Choice Send [(Msg "xx", RecCall (Var rec))] ]
    else elements [RecCall (Var rec)]

recterm :: String -> Bool -> Int -> Gen SessionType
recterm s flag 0 = elements [RecCall (Var s)]
recterm s flag n = do
  msgs <- fmap nub $ listOf1 (resize 10 arbitrary) :: Gen [Message]
  nexts <- vectorOf (length msgs) (recterm s False (n-1))  :: Gen [SessionType]
  dir <- arbitrary
  let pairs = zip msgs nexts
  elements [ if flag 
             then Choice dir pairs
             else RecCall (Var s)
           , Choice dir pairs
           ]
    
maxTerm :: [String] -> SessionType    
maxTerm xs = helper xs xs
  where helper (x:xs) all = RecDef (Var x) $ Choice Send [(Msg $ map toLower x, (helper xs all))]
        helper [] all = Choice Send $ map (\x -> (Msg $ map toLower x, RecCall (Var x))) all


makeMaxTerms :: ([String] -> SessionType) -> Int -> [((Int, Int), SessionType)]
makeMaxTerms f 0 = []
makeMaxTerms f i = let alphabet = [[x]++[y] | x <- ['A'..'Z'], y <- ['A'..'Z'] ]
                       list = take i alphabet
                   in ((length list,length list), f list ):(makeMaxTerms f (i-1))



maxBranchTerm :: Direction -> [String] -> SessionType    
maxBranchTerm dir xs = helper xs xs
  where helper (x:xs) all = RecDef (Var x) $ Choice dir [(Msg $ map toLower x, (helper xs all))]
        helper [] all = Choice dir $ map (\x -> (Msg $ map toLower x, 
                                                 Choice dir $ map (\y -> (Msg $ map toLower y, RecCall (Var x))) all
                                                 )) all

singleRec :: String -> [String] -> SessionType 
singleRec s xs = RecDef (Var s) $ Choice Send $ map (\y -> (Msg y, RecCall (Var s))) xs


-- main :: IO ()
-- main = do
--   saveTypes $ makeMaxTerms (maxBranchTerm Send) 100
--   putStrLn "Done"


main :: IO ()
main = 
  if True 
  then do -- 
          list <- sample' (resize 5 arbitrary :: Gen SessionType)
          let sizes = map (\x -> (maxDepth x, numberOfMessages x)) list
          putStrLn $ show sizes
          saveTypes $ zip sizes list
          
  else do list' <- sample' (recterm "X" True 6 :: Gen SessionType)
          let list = map (\x -> RecDef (Var "X") x) list'
          let sizes = map (\x -> (maxDepth x, numberOfMessages x)) list
          putStrLn $ show sizes
          saveUnfold $ zip sizes list

saveTypes :: [((Int, Int), SessionType)] -> IO ()
saveTypes sts = helper 0 sts 
  where helper i (((d,w),x):xs) = 
          do time <- getCurrentTime
             let f = "generated_test_"++(show i)++"_"++(show d)++"x"++(show w)
                     ++"__"++((replace " " "") $ show $ time)++".txt"
             writeFile f (printSessionType x)
             helper (i+1) xs
        helper i [] = return ()
        
        
saveUnfold :: [((Int, Int), SessionType)] -> IO ()
saveUnfold sts = helper 0 sts 
  where helper i (((d,w),x):xs) = 
          do time <- getCurrentTime
             let f = "generated_test_"++(show i)++"_"++(show d)++"x"++(show w)
                     ++"__"++((replace " " "") $ show $ time)
             writeFile (f++".txt") (printSessionType x)
             writeFile (f++"_unfolded.txt")  (printSessionType $ unfold 1 x)
             helper (i+1) xs
        helper i [] = return ()
        
        
    
unfold :: Int -> SessionType -> SessionType        
unfold i (RecDef s t)  = RecDef s (helper i t)
  where helper n (RecCall s) 
          | n > 1  = helper (n-1) t
          | n <= 1 = t
        helper n (Choice dir xs) = Choice dir (map (\x -> (fst x, helper n $ snd x)) xs)
        helper n _ = error $ "Unsupported unfolding"
unfold _ _ = error $ "Unsupported unfolding"

wellSized :: Int -> SessionType -> Bool
wellSized i lt = ((i-2) <= maxDepth lt) && (maxDepth lt <= (i+2))
            
maxDepth :: SessionType -> Int
maxDepth End = 0
maxDepth (RecCall _) = 0
maxDepth (RecDef _ t) = (maxDepth t)
maxDepth (Choice dir xs) = 1+(maximum $ map (maxDepth . snd) xs)

numberOfLeaves :: SessionType -> Int
numberOfLeaves End = 1
numberOfLeaves (RecCall _) = 1
numberOfLeaves (RecDef _ t) = (numberOfLeaves t)
numberOfLeaves (Choice dir xs) = foldr (+) 0 $ map (numberOfLeaves . snd) xs


numberOfMessages :: SessionType -> Int
numberOfMessages End = 0
numberOfMessages (RecCall _) = 0
numberOfMessages (RecDef _ t) = (numberOfMessages t)
numberOfMessages (Choice dir xs) = foldr (+) (length xs) $ map (numberOfMessages . snd) xs

    
printDirection :: Direction -> String
printDirection Send = "!"
printDirection Receive = "?"

printSessionType :: SessionType -> String
printSessionType End = "end"
printSessionType (RecCall (Var var)) = var
printSessionType (RecDef (Var var) t) = "rec "++var++" . "++(printSessionType t)
printSessionType (Choice dir xs) = (if dir == Send
                                    then "+{"
                                    else "&[")
                                   ++
                                   helper dir xs
                                   ++
                                   (if dir == Send
                                    then "}"
                                    else "]")
  where helper dir ((Msg m,x):y:xs) = (printDirection dir)++(m)++"; "++(printSessionType x)
                                    ++",\n"++(helper dir (y:xs))
        helper dir [(Msg m,x)] = (printDirection dir)++(m)++"; "++(printSessionType x)
        helper _ [] = []
        
