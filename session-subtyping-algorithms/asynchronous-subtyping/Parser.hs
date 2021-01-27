module Parser where

import System.Environment
import System.FilePath.Posix
import Control.Applicative((<*))
import Data.List as L
import Data.Foldable as F
import Data.Tree as T
import Data.Maybe (mapMaybe)
import Data.Map (Map, empty, map, insert, unionWith, unionsWith, singleton,(!),toList)

-- Parser
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language (emptyDef)



-- Pretty Printing
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (Doc, (<>), (<+>))



type Environment = Map String (Int, LocalType)

data Direction = Send | Receive
               deriving (Eq, Ord, Read)


instance Show Direction where
  show Send = "!"
  show Receive = "?"

data LocalType = Act Direction String LocalType     -- Send/Receive prefix
               | Rec String LocalType      -- Recursive def
               | Var String                -- Recursive call
               | End                       -- End
               | Choice Direction [LocalType]
               deriving (Eq, Ord, Read)



-- instance Ord LocalType where
--   compare t1 t2 = compare (numberOfTransitions t1) (numberOfTransitions t2)

instance Show LocalType where
  show = printLocalType


dual :: Direction -> Direction
dual Send = Receive
dual Receive = Send


dualType :: LocalType -> LocalType
dualType (Act dir s t) = (Act (dual dir) s (dualType t))
dualType (Choice dir xs) = (Choice dir $ L.map dualType xs)
dualType (Rec s t) = Rec s (dualType t)
dualType (Var s) = (Var s)
dualType End = End


getPairs :: [LocalType] -> [(String, LocalType)]
getPairs ((Act Send s lt):xs) = (string2send s, lt):(getPairs xs)
getPairs ((Act Receive s lt):xs) = (string2receive s, lt):(getPairs xs)
getPairs [] = []
getPairs lt = error $ "Ill-formed type: " ++(show lt)

printLabel :: String -> String
printLabel s = if (take 4 s) == "Send"
               then drop 4 s
               else drop 7 s


string2send :: String -> String
string2send s = "Send"++s

string2receive :: String -> String
string2receive s = "Recv"++s


string2parent :: (String -> String) -> String -> String
string2parent f s = act2parent $ f s

act2parent :: String -> String
act2parent s = ("Parent"++s)

parent2act :: String -> String
parent2act s = drop 6 s


allActions :: LocalType -> ([String],[String])
allActions (Act Send s lt) = let (snd, rcv) = allActions lt
                             in ((string2send s):snd, rcv)
allActions (Act Receive s lt) = let (snd, rcv) = allActions lt
                                in (snd, (string2receive s):rcv)
allActions (Rec var lt)  = allActions lt
allActions (Var _) = ([],[])
allActions End = ([],[])
allActions (Choice _ xs) = L.foldr (\(s1,r1) -> \(s2,r2) -> (s1++s2,r1++r2)) ([],[]) (L.map allActions xs)

freeVariables :: LocalType -> [String]
freeVariables (Act dir s t) = freeVariables t
freeVariables (Choice dir xs) = nub $ L.foldr (++) [] $ L.map freeVariables xs
freeVariables (Var s) = [s]
freeVariables End = []
freeVariables (Rec s t) = delete s $ freeVariables t

getNextSnd :: [LocalType] -> [LocalType]
getNextSnd xs = L.map (\(Act Send s lt) -> lt) xs


isSend :: LocalType -> Bool
isSend (Act Send s lt) = True
isSend _ = False

isReceive :: LocalType -> Bool
isReceive (Act Receive _ lt) = True
isReceive _ = False

disjointPrefix :: [LocalType] -> Bool
disjointPrefix list = let l = helper list in (length l) == (length $ nub l)
  where helper ((Act _ s _):xs) = s:(helper xs)
        helper _ = []

isExtChoice :: [LocalType] -> Bool
isExtChoice [] = False
isExtChoice (x:xs) = isReceive x

isIntChoice :: [LocalType] -> Bool
isIntChoice [] = False
isIntChoice (x:xs) = isSend x

wellFormed :: LocalType -> Bool
wellFormed = wellFormed_ []
  where wellFormed_ vars (Rec s lt) = (s `L.notElem` vars) && (wellFormed_ (s:vars) lt)
        wellFormed_ vars (Var s) = L.elem s vars
        wellFormed_ vars (Act Send _ lt) = wellFormed_ vars lt
        wellFormed_ vars (Act Receive _ lt) = wellFormed_ vars lt
        wellFormed_ _ End = True
        wellFormed_ vars (Choice Send list) = (F.and $ L.map isSend list)
                                           && (disjointPrefix list)
                                           && (F.and $ L.map (wellFormed_ vars) list)
        wellFormed_ vars (Choice Receive list) = (F.and $ L.map isReceive list)
                                            && (disjointPrefix list)
                                            && (F.and $ L.map (wellFormed_ vars) list)


typeDepth :: LocalType -> Int
typeDepth (Act dir s t) = 1+(typeDepth t)
typeDepth (Choice dir xs) = let l = L.map typeDepth xs
                            in if L.null l then 0
                               else L.maximum l
typeDepth (Var s) = 1
typeDepth End = 1
typeDepth (Rec s t) = 1+(typeDepth t)


numberOfLeaves :: LocalType -> Int
numberOfLeaves End = 1
numberOfLeaves (Var _) = 1
numberOfLeaves (Rec _ t) = (numberOfLeaves t)
numberOfLeaves (Choice dir xs) = L.foldr (+) 0 $ L.map numberOfLeaves xs
numberOfLeaves (Act dir s t) = (numberOfLeaves t)


numberOfTransitions :: LocalType -> (Int, Int)
numberOfTransitions (Act dir s t) =
  let (s,r) = numberOfTransitions t
  in if dir==Send
     then (1+s, r)
     else (s, 1+r)
numberOfTransitions (Choice dir xs) =  L.foldr (\(x,x') (y,y') -> (x+y,x'+y')) (0,0) $ L.map numberOfTransitions xs
numberOfTransitions (Var s) = (0,0)
numberOfTransitions End = (0,0)
numberOfTransitions (Rec s t) = numberOfTransitions t

average xs = realToFrac (L.sum xs) / genericLength xs

-- (printf "%.4f" (averageDegree ts))
branchingFactor :: LocalType -> Float
branchingFactor t = case helper t of
  Just x -> x
  Nothing -> 0
  where
    helper :: LocalType -> Maybe Float
    helper (Act dir s t) = helper t
    helper (Rec s t) = helper t
    helper (Choice dir xs) =
      Just $ average $
      (fromIntegral $ length xs):((mapMaybe helper xs))
    helper (Var _) = Nothing
    helper End = Nothing

numberOfRecursions :: LocalType -> Int
numberOfRecursions (Act dir s t) = (numberOfRecursions t)
numberOfRecursions (Choice dir xs) =  let l = L.map numberOfRecursions xs
                                      in if L.null l then 0
                                         else L.maximum l
  -- L.foldr (+) 0 $ L.map numberOfRecursions xs
numberOfRecursions (Var s) = 0
numberOfRecursions End = 0
numberOfRecursions (Rec s t) = (if s `L.elem` (freeVariables t)
                                then 1
                                else 0)
                               + (numberOfRecursions t)

firstRecursion :: LocalType -> Int
firstRecursion (Act dir s t) = 1+(firstRecursion t)
firstRecursion (Choice dir xs) = let l = L.map firstRecursion xs
                                 in if L.null l then 0
                                    else L.minimum l
firstRecursion (Var s) = 0
firstRecursion End = 0
firstRecursion (Rec s t) = 0


maxRecursion :: LocalType -> (String, Int)
maxRecursion t = let fmax (_,i) (_,j) = compare i j
                     l = Data.Map.toList $ recursionDistance t Data.Map.empty
                 in if L.null l then ("Nothing", 0)
                    else L.maximumBy fmax l


recursionDistance :: LocalType -> Map String Int -> Map String Int
recursionDistance (Act dir s t) m =  recursionDistance t (Data.Map.map (1+) m)
recursionDistance (Choice dir xs) m =  unionsWith (\x y -> L.maximum [x,y]) $ L.map (flip recursionDistance m) xs
recursionDistance (Var s) m = Data.Map.singleton s (m!s)
recursionDistance End m = Data.Map.empty
recursionDistance (Rec s t) m = recursionDistance t (Data.Map.insert s 0 m)



varOccurences :: String -> LocalType -> Int
varOccurences s End = 0
varOccurences v (Act dir s t) = varOccurences v t
varOccurences v (Choice dir xs) = L.foldr (+) 0 (L.map (varOccurences v) xs)
varOccurences v (Rec s t) = if s/=v then varOccurences v t
                            else 0
varOccurences v (Var s) = if s == v then 1
                          else 0

sizeUnfolded :: [String] -> LocalType -> Int
sizeUnfolded vars End = 0
sizeUnfolded vars (Var s) = 0
sizeUnfolded vars (Act dir s t) = 1+(sizeUnfolded vars t)
sizeUnfolded vars (Choice dir xs) = L.foldr (+) 0 (L.map (sizeUnfolded vars) xs)
sizeUnfolded vars r@(Rec s t) = (1 + (varOccurences s t)) * (sizeUnfolded (s:vars) t)

-- "var" to be replaced by "t" in "(...)"
substitute :: String -> LocalType -> LocalType -> LocalType
substitute var t p = substituteList [var] t p

substituteList :: [String] -> LocalType -> LocalType -> LocalType
substituteList [] t p = p
substituteList var t End = End
substituteList var t (Var v) = if v `L.elem` var then t
                               else (Var v)
substituteList var t (Rec s t') =
  if s `L.elem` var
  then Rec s (substituteList var t t')
  else Rec s (substituteList (delete s var) t t')
substituteList var t (Choice dir xs) = Choice dir (L.map (substituteList var t) xs)
substituteList var t (Act dir s t') = Act dir s (substituteList var t t')



-- Lexer & Parser
lexer :: T.TokenParser ()
lexer = T.makeTokenParser  languageDef

languageDef =
  emptyDef { T.commentStart    = "/*"
           , T.commentEnd      = "*/"
           , T.commentLine     = "--"
           , T.identStart      = alphaNum
           , T.identLetter     = alphaNum
           , T.reservedNames   = []
           , T.reservedOpNames = ["!", "?", "+", "&"]
           , T.caseSensitive = True
           }

whiteSpace= T.whiteSpace lexer
lexeme    = T.lexeme lexer
symbol    = T.symbol lexer
parens    = T.parens lexer
identifier= T.identifier lexer

ltparser :: Parser LocalType
ltparser = do { symbol "!"
              ; act <-  identifier
              ; symbol ";"
              ; cont <- ltparser
              ; return $ Act Send act cont
              }
           <|>
           do { symbol "?"
              ; act <-  identifier
              ; symbol ";"
              ; cont <- ltparser
              ; return $ Act Receive act cont
              }
           <|>
           do { symbol "rec"
              ; var <-  identifier
              ; symbol "."
              ; cont <- ltparser
              ; return $ Rec var cont
              }
           <|>
           do { symbol "end"
              ; return  End
              }
           <|>
           do { var <-  identifier
              ; return $ Var var
              }
           <|> -- REDUNDANCY TO MAKE IT EASIER TO TYPE TYPES
           do { symbol "{"
              ; list <- sepBy1 ltparser (char ',' <* spaces)
              ; symbol "}"
              ; return $ Choice (if (isExtChoice list) then Receive else Send) list
              }
           <|>
           do { symbol "["
              ; list <- sepBy1 ltparser (char ',' <* spaces)
              ; symbol "]"
              ; return $ Choice (if (isIntChoice list) then Send else Receive) list
              }
           <|>
           do { dir <- (symbol "+" <|> symbol "&")
              ; choice <- choiceParser
              ; return choice
              }

choiceParser =
  do { symbol "["
     ; list <- sepBy1 ltparser (char ',' <* spaces)
     ; symbol "]"
     ; return $ Choice (if (isIntChoice list) then Send else Receive) list
     }
  <|>
  do { symbol "{"
     ; list <- sepBy1 ltparser (char ',' <* spaces)
     ; symbol "}"
     ; return $ Choice (if (isIntChoice list) then Send else Receive) list
     }

mainparser :: Parser LocalType
mainparser =  whiteSpace >> ltparser <* eof



parseLocalType :: String -> Either ParseError LocalType
parseLocalType inp =  parse mainparser "" inp



printDirection :: Direction -> String
printDirection Send = "!"
printDirection Receive = "?"



printLocalType :: LocalType -> String
printLocalType (Act dir s lt) = (printDirection dir)++s++"; "++(printLocalType lt)
printLocalType (Rec s lt) = "rec "++s++" . "++(printLocalType lt)
printLocalType End = "end"
printLocalType (Var s) =  s
printLocalType (Choice dir xs) = (if dir == Send
                                  then "+{"
                                  else "&[")
                                 ++
                                 helper xs
                                 ++
                                 (if dir == Send
                                  then "}"
                                  else "]")
  where helper (x:y:xs) = (printLocalType x)
                          ++", "++(helper (y:xs))
        helper [x] = printLocalType x
        helper [] = []

localTypeToTree :: LocalType -> Tree String
localTypeToTree (Act dir s lt) = Node ((printDirection dir)++ s) [localTypeToTree lt]
localTypeToTree (Rec s lt) = Node ("Rec "++s) [localTypeToTree lt]
localTypeToTree End = Node "end" []
localTypeToTree (Var s) =  Node s []
localTypeToTree (Choice dir xs) = Node ((printDirection dir)++" choice") (L.map localTypeToTree xs)


printTypeTree :: LocalType -> String
printTypeTree f = drawTree $ localTypeToTree f



reduce :: LocalType -> LocalType
reduce t = helper Nothing t
  where helper b (Rec s t) = case b of
          Just x -> helper b (substitute s (Var x) t)
          Nothing -> Rec s (helper (Just s) t)
        helper b (Choice dir xs) = Choice dir (L.map (helper b ) xs)
        helper b (Act dir s t) = Act dir s (helper Nothing t)
        helper b t = t
