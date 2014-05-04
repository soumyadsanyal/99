import Data.Char (digitToInt)
import Data.Char (toUpper)
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))
 
 
--Lemmas

empty :: [a] -> Bool
empty [] = True
empty _ = False

listadder :: [a] -> [a] -> [a]
listadder lista listb
 | empty lista = listb
 | otherwise = x: (listadder rest listb)
 where
  (x:rest) = lista


top :: [a] -> a
top list 
 | (empty list) = error "Empty list, silly!"
 | otherwise = x 
 where 
  (x:rest) = list


remainder :: [a] -> [a]
remainder list 
 | (empty list) = error "Empty list, silly!"
 | otherwise = rest 
 where 
  (x:rest) = list


cat :: [[a]] -> [a]
cat list
 | empty list = []
 | otherwise = listadder x (cat rest)
 where
  (x:rest) = list



ender :: [a] -> [a]
ender list
 | empty list = error "Empty list, silly!"
 | otherwise = rest
 where
  (x:rest) = list

conjunct :: [Bool] -> Bool
conjunct list
 | empty list = error "Empty list!"
 | rest==[] = x
 | otherwise = (x && (conjunct rest))
 where
  (x:rest)=list

disjunct :: [Bool] -> Bool
disjunct list
 | empty list = error "Empty list, silly!"
 | rest == [] = x
 | otherwise = (x || (disjunct rest))
 where
  (x:rest) = list

isIn :: Eq a => a -> [a] -> Bool
isIn thing list
 | empty list = False
 | x == thing = True
 | otherwise = isIn thing rest
 where
  (x:rest) = list

isNotIn :: Eq a => a -> [a] -> Bool
isNotIn thing list
 | empty list = True
 | x == thing = False
 | otherwise = isNotIn thing rest
 where
  (x:rest) = list


taker :: Int -> [a] -> [a]
taker number list
 | number < 0 = error "I'm not a giver, I'm a taker!"
 | number == 0 = []
 | otherwise = x: (taker (number - 1) rest)
 where
  (x:rest) = list 

dropper :: Int -> [a] -> [a]
dropper number list
 | number < 0 = error "I'm not a giver, I'm a dropper!"
 | number ==0 = list
 | number>0 = dropper (number - 1) rest
 where
  (x:rest) = list 

splitInPlace :: Int -> [a] -> ([a],[a])
splitInPlace index list 
 | index < 0 || index > (longer list) = error "You're out of line, man!"
 | otherwise = ((taker index list),(dropper index list))

takeSoLongAsTrue :: (a->Bool) -> [a] -> [a]
takeSoLongAsTrue predicate list
 | ((predicate x) == False) = []
 | otherwise = x: (takeSoLongAsTrue predicate rest)
 where
  (x:rest) = list

dropAfterFirstFalse:: (a->Bool) -> [a] -> [a]
dropAfterFirstFalse predicate list
 | (predicate x) == True = dropAfterFirstFalse predicate rest
 | otherwise = list
 where
  (x:rest) = list

dropAtFirstFalse :: (a->Bool) -> [a] -> [a]
dropAtFirstFalse predicate list 
 | (predicate x) == False = list
 | otherwise = dropAtFirstFalse predicate rest
 where
  (x:rest) = list

--whiletaker :: (a->Bool) -> [a] -> [a]
--whiletaker predicate list 
-- | (predicate x) == False = []
-- | otherwise = x: (whiletaker predicate rest)
-- where
--  (x:rest) = list

breakright :: (a->Bool) -> [a] -> [a]
breakright predicate list 
 | (predicate x) == True = list
 | otherwise = breakright predicate rest
 where
  (x:rest) = list

breakleft :: (a->Bool) -> [a] -> [a]
breakleft predicate list 
 | (predicate x) == True = []
 | otherwise = x: (breakleft predicate rest)
 where
  (x:rest) = list


--This no longer works, I renamed the iftaker function
--spanner :: (a->Bool) -> [a] -> ([a],[a])
--spanner predicate list
-- | empty list = error "You can't give me nothin', man!"
-- | otherwise = (iftaker predicate list, ifdropper predicate list)

breaker :: (a->Bool) -> [a] -> ([a],[a])
breaker predicate list
 | empty list = error "You can't give me nothing, man!"
 | otherwise = (breakleft predicate list, breakright predicate list)


filtering :: (a->Bool) -> [a] -> [a]
filtering predicate list
 | empty list = []
 | predicate x == True = x : (filtering predicate rest)
 | otherwise = filtering predicate rest
 where
  (x:rest) = list

isStartOf :: Eq a => [a] -> [a] -> Bool
isStartOf thing problems
 | empty problems = error "What's your problem?!"
 | empty thing = True
 | not (thispartofthing == thispartofproblems) = False
 | restofthing  == [] = True
 | otherwise = isStartOf restofthing restofproblems
 where
  (thispartofthing:restofthing) = thing
  (thispartofproblems:restofproblems) = problems

isEndOf :: Eq a => [a] -> [a] -> Bool
isEndOf thing problems = isStartOf (rev thing) (rev problems)

-- I should be able to find an optimization of this function:
isPartOf :: Eq a => [a] -> [a] -> Bool
isPartOf thing problems
 | isStartOf thing problems = True
 | restofproblems == [] = False
 | otherwise = isStartOf thing restofproblems
 where
  (thispartofproblems:restofproblems) = problems

zipper :: Eq a => Eq b => [a] -> [b] -> [(a,b)]
zipper lista listb
 | lista == [] || listb == [] = []
 | otherwise = (thispartoflista,thispartoflistb): (zipper restoflista restoflistb)
 where
  (thispartoflista:restoflista) = lista
  (thispartoflistb:restoflistb) = listb

zipperWith :: Eq a => Eq b =>  (a->b->c) -> [a] -> [b] -> [c]
zipperWith function lista listb
 | lista == [] || listb == [] = []
 | otherwise = (function thispartoflista thispartoflistb) : (zipperWith function restoflista restoflistb)
 where
  (thispartoflista:restoflista) = lista
  (thispartoflistb:restoflistb) = listb

splitlines ::  String -> Bool
splitlines string
 | x=='\n'  = True 
 | rest=="" = False
 | otherwise = splitlines rest
 where
  (x:rest) = string


leftOf:: (a->Bool) -> [a] -> [a]
leftOf predicate list 
 | (predicate x) == True = []
 | otherwise = x: (leftOf predicate rest)
 where
  (x:rest) = list

rightOf :: (a->Bool) -> [a] -> [a]
rightOf predicate list 
 | (predicate x) == True = rest
 | otherwise = (rightOf predicate rest)
 where
  (x:rest) = list


splitter string 
 | isNotIn '\n' string  = string:[]
 | otherwise = listadder ((leftOf (=='\n') string) : []) (splitter rest)
 where
  rest = rightOf (=='\n') string

isWhitespace character = isIn character whitelist
 where
  whitelist = ['\n','\t',' ']

containsWhitespace string = isIn '\n' string  || isIn '\t' string || isIn ' ' string


totalsplitter string 
 | not (containsWhitespace string)  = string:[]
 | otherwise = listadder ((leftOf (isWhitespace) string) : []) (totalsplitter rest)
 where
  rest = rightOf (isWhitespace) string

safetop :: [a] -> Maybe a
safetop list 
 | (empty list) = Nothing
 | otherwise = Just x 
 where 
  (x:rest) = list


loop acc [] = acc
loop acc (x:rest) = loop acc' rest
 where
  acc' = 10*acc + digitToInt x

asInt :: String -> Int
asInt xs = loop 0 xs

squarer :: Num a => [a] -> [a]
squarer list
 | empty list = []
 | otherwise = (x*x) : (squarer rest)
 where
  (x:rest) = list

mapper :: (a->b) -> [a] -> [b]
mapper function list
 | empty list = []
 | otherwise = (function x) : (mapper function rest)
 where
  (x:rest)=list

composer :: (b->c)->(a->b)->(a->c)
composer f g = \x-> (f (g x))

accsummer :: Num a => [a] -> a
accsummer list = helper 0 list
 where
  helper acc (x:rest) = helper (acc+x) rest
  helper acc _ = acc


lfolder :: (a->b->a)->a->[b]->a
lfolder function accumulator list
 | empty list = accumulator
 | otherwise = lfolder function (function accumulator x) rest
 where
  (x:rest) = list

rfolder :: (lt->acc->acc)->acc->[lt]->acc
rfolder function accumulator list
 | empty list = accumulator
 | otherwise = function x (rfolder function accumulator rest)
 where
  (x:rest) = list

filterer:: (lt->Bool)->[lt]->[lt]
filterer predicate (x:rest) = rfolder function accumulator (x:xs)
 where
  function lt acc
   |predicate lt = lt:acc
   |otherwise = acc




--Here begin the theorems

--Theorem 1

finaller :: [a] -> a
finaller list
 | empty list = error "Empty list, silly!"
 | (not (empty xs)) = finaller xs
 | (empty xs) = x
 | otherwise = error "Invalid arguments!"
 where
  (x:xs) = list

--Theorem 2

secondtofinaller :: [a] -> a
secondtofinaller list
 | empty list = error "Empty list, silly!"
 | (not (empty rest)) = secondtofinaller (y:rest)
 | (empty rest) = x
 | otherwise = error "Invalid arguments!"
 where
  (x:y:rest) = list

--Theorem 3

kelement :: Int -> [a] -> a
kelement place list 
 | place < 1 = error "Indexing starts at 1, silly!"
 | empty list = error "List too short, silly!"
 | place==1 = x
 | otherwise = kelement (place-1) rest
 where
  (x:rest) = list

--Theorem 4 

longer :: [a] -> Int
longer list
 | empty list = 0
 | otherwise = 1+(longer rest)
 where
  (x:rest) = list

--Theorem 5


rev:: [a] -> [a]
rev list 
 | empty list = []
 | otherwise = listadder (rev rest) [x]
 where
  (x:rest)=list

--Theorem 6

ifpalindrome :: (Eq a)=> [a] -> Bool
ifpalindrome string
 | string == (rev string) = True
 | otherwise = False


 
