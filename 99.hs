 
import Data.Char (digitToInt)
import Data.Char (toUpper)
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))
--import System.Random
import Control.Monad
 
 
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
filterer predicate (x:rest) = rfolder function [] (x:rest)
 where
  function lt acc
   |predicate lt = lt:acc
   |otherwise = acc

identity :: [a] -> [a]
identity list = rfolder (:) [] list

listappender :: [a]->[a]->[a]
listappender lista listb = rfolder (:) listb lista

secondcoordinate :: (a,b)->b
secondcoordinate (x,y) = y

firstcoordinate :: (a,b) -> a
firstcoordinate (x,y) = x

--Exploring type construction

data Move = Lefter | Righter | Upper | Downer  deriving (Eq, Show)
 
type Pos = (Int,Int) 

move :: Move->Pos->Pos
move direction (x,y)
 | direction == Lefter = (x-1,y)
 | direction == Righter = (x+1,y)
 | direction == Upper = (x,y+1)
 | direction == Downer = (x,y-1)

moves :: [Move]->Pos->Pos
moves directions (x,y)
 | empty directions = (x,y)
 | otherwise = moves rest (move direction (x,y))
 where
  (direction:rest) = directions

data Shape = Circle Pos Float | Rectangle Pos Float Float
 deriving (Eq, Show)

area :: Shape -> Float
area (Circle pos rad) = pi*rad*rad
area (Rectangle pos up right) = up*right

anotherarea :: Shape -> Float
anotherarea shape = case shape of
                  (Circle pos rad) -> rad*rad*pi
                  (Rectangle pos right up) -> right*up

data Nat = Zero | Succ Nat
 deriving (Show, Eq)

natnormal :: Nat -> Int
natnormal natural = case natural of
                         Zero -> 0
                         (Succ n) -> 1 + natnormal n

natweird :: Int -> Nat
natweird integer = case integer of
                        0 -> Zero
                        n -> Succ (natweird (n-1))

natadder :: Nat->Nat->Nat
natadder this that = case this of
                     Zero -> that
                     (Succ n) ->  Succ (natadder n that)




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


--Theorem 7

data Nested a = Element a | List [Nested a]
 deriving (Show)

flatten :: [Nested a] -> [a]
flatten list = case list of
                    ((Element term):[]) -> term:[]
                    ((Element term):rest) -> term:(flatten rest)
                    ((List expression):[]) -> flatten expression
                    ((List expression):rest) -> listadder (flatten expression) (flatten rest)


--Theorem 8


compressor :: Eq a => [a]->[a]
compressor list = rfolder function accumulator list
 where
  function term list
   | empty list = term:[]
   | term == (top list) = list
   | otherwise = term:list
  accumulator = []

--Theorem 9

compressinator :: Eq a =>  [a]->[[a]]
compressinator list = rfolder function accumulator list
 where
  accumulator = []
  function :: Eq a => a->[[a]]->[[a]]
  function term list
   | empty list = (term:[]):[]
   | isIn term x = (term:x):rest
   | otherwise = (term:[]):list
   where
    (x:rest) = list


--Theorem 10

countingCompressor :: Eq a => Num b => [a] -> [(b,a)]
countingCompressor list = rfolder function accumulator list
 where
  accumulator = []
  function :: Eq a => Num b => a->[(b,a)]->[(b,a)]
  function term list
   | empty list = (1,term):list
   | secondcoordinate x == term = let (currentnumber,expression)=x
                                  in (currentnumber+1,expression):rest
   | otherwise = (1,term):list
   where
    (x:rest) = list

-- Theorem 11

data Numbers a = Single a | Multiple Int a
 deriving (Show, Eq)

extractValue :: Numbers a -> a
extractValue expression = case expression of
                              (Multiple num term) -> term
                              (Single term) -> term

extractNum :: Numbers a -> Int 
extractNum expression = case expression of
                            (Multiple num term) -> num
                            (Single term) -> 1

countingCompressor' :: Eq a => [a] -> [Numbers a]
countingCompressor' list = rfolder function accumulator list
 where
  accumulator = []
  function :: Eq a => a->[Numbers a]->[Numbers a]
  function term list
   | empty list = (Single term):list
   | ((extractValue x) == term) = case x of
                                     (Single term) -> (Multiple 2 term):rest
                                     (Multiple currentnumber term) -> (Multiple (currentnumber+1) term):rest

   | otherwise = (Single term):list
   where
    (x:rest) = list
    

-- Theorem 12


decode :: [Numbers a] -> [a]
decode expression = case expression of
                         [] -> []
                         ((Single term):rest) -> term:(decode rest)
                         ((Multiple num term):rest) -> case num of
                                                            2 -> term:(decode ((Single term):rest))
                                                            _ -> term:(decode ((Multiple (num-1) term):rest))

-- Theorem 13

-- See Theorem 11

-- Theorem 14

multiplier :: Int -> a -> [a]
multiplier num term
 | num == 0 = []
 | otherwise = term:(multiplier (num-1) term)


duplicate :: [a]-> [a]
duplicate expression = case expression of
                            [] -> []
                            (x:rest) -> listadder (multiplier 2 x) (duplicate rest)

-- Theorem 15

kplicate :: Int -> [a] -> [a]
kplicate num expression = case expression of
                               [] -> []
                               (x:rest) -> listadder (multiplier num x) (kplicate num rest)

-- Theorem 16

droppinator period 0 (x:rest) = droppinator period (period-1) rest
droppinator period currentperiod (x:rest) = x:(droppinator period (currentperiod-1) rest)
droppinator _ _ [] = []

dropAndGiveMe16 period expression = droppinator period (period-1) expression

-- Theorem 17

splittHelper :: Int -> [[a]] -> [[a]]
splittHelper 0 expression = expression
splittHelper num (first:second:[]) = splittHelper (num-1) ((listadder first (topsecond:[])):(restsecond):[])
 where
  (topsecond:restsecond) = second

splitt :: Int -> [a] -> [[a]]
splitt num expression = splittHelper num (listadder [[]] (expression:[]))

--Theorem 18

slicer :: Int -> Int -> [a] -> [a]
slicer 1 end expression = x
 where
  (x:rest) = (splitt end expression)
slicer i j (x:rest) = slicer (i-1) (j-1) rest

--Theorem 19

rotater :: Int -> [a] -> [a]
rotater num expression = if (not (empty expression)) then listadder (rev x) (rev y) else []
 where 
  (x:y:rest) = splitt (mod num (longer expression)) (rev expression)

-- I really should write the mod function from scratch. For now this will do.

-- Theorem 20

pointDrop :: Int -> [a] -> [a]
pointDrop num expression = listadder first restofsecond
 where
  first:second:[] = splitt (num-1) expression
  topsecond:restofsecond = second

-- Theorem 21

pointInsert :: Int -> a -> [a] -> [a]
pointInsert num term expression = listadder (listadder first (term:[])) second
 where
  first:second:[] = splitt (num-1) expression

-- Theorem 22

range :: Int -> Int -> [Int]
range start stop = [start..stop]

--Theorem 23

--I need to learn something about the System.Random API before I do this problem.

--Theorem 24

--I need to learn something about the System.Random API before I do this problem.

--Theorem 25

--I need to learn something about the System.Random API before I do this problem.

--Theorem 26 

starter string
 | empty string = []
 | otherwise = False:(starter rest)
 where 
  (first:rest) = string

flipOn :: [Bool] -> Int->Int->[Bool]
flipOn string index length 
 | empty string = []
 | index==1 = True:rest
 | 1<=index && index<=length = False:(flipOn rest (index-1) length )
 where
  (x:rest) = string

seedHelper :: [a]->Int->Int->[[Bool]]
seedHelper string index length
 | empty string = []
 | index>length = []
 | otherwise = (flipOn base index length):(seedHelper base (index+1) length)
 where
  base = starter string
  (x:rest)=string

seed :: [a] -> [[Bool]]
seed string = seedHelper string 1 (longer string)

bitwiseOr first second
 | (longer first)/=(longer second) = error "Oh no!"
 | empty first || empty second = []
 | otherwise = (topfirst || topsecond):(bitwiseOr restfirst restsecond)
 where
  (topfirst:restfirst)=first
  (topsecond:restsecond)=second

irredundant :: Eq a => [[a]]->[[a]]
irredundant list
 | empty list = []
 | otherwise = if (isIn x rest) then (irredundant rest) else (x:(irredundant rest))
 where
  (x:rest)=list

build :: Int->[a]->[[Bool]]
build 1 string = seed string
build level string = irredundant [bitwiseOr first second | first<-(build (level-1) string), second<- (build 1 string)]

onlyLevel :: Int->[a]->[[Bool]]
onlyLevel level string = [list | list<-(build level string), not (isIn list (build (level-1) string))]

mapFromBoolToSubString :: [Bool]->[a]->[a]
mapFromBoolToSubString switches string
 | (longer switches)/=(longer string) = error "Oh no!"
 | empty switches || empty string = []
 | otherwise = if firstswitches==True then firststring:(mapFromBoolToSubString restswitches reststring) else mapFromBoolToSubString restswitches reststring
 where
  (firststring:reststring)=string
  (firstswitches:restswitches)=switches

combinations :: Int->[a]->[[a]]
combinations level string = [mapFromBoolToSubString term string | term<-(onlyLevel level string)]

-- There's got to be a better algorithm for this.

--Theorem 27

bitwiseNot :: [Bool]->[Bool]
bitwiseNot string 
 | empty string = []
 | otherwise = (not x):(bitwiseNot rest)
 where
  (x:rest) = string

dropSelected :: [a] -> [Bool] -> [a]
dropSelected string switches
 | empty string = []
 | otherwise = if thisswitches==True then dropSelected reststring restswitches else thisstring:(dropSelected reststring restswitches)
 where
  (thisstring:reststring) = string
  (thisswitches:restswitches) = switches

-- This will be recursively split up by subset using 26





--Theorem 28


listInserter :: [a]->[[a]]->[[a]]
listInserter list [] = list:[]
listInserter list (firstlist:restlist)
 | (longer list)>(longer firstlist) = listadder (firstlist:[]) (listInserter list restlist)
 | otherwise = list:(firstlist:restlist)



listsorter::[[a]]->[[a]]
listsorter metalist = rfolder function accumulator metalist
 where
  function list metalist = listInserter list metalist
  accumulator = []

 
--Theorem 31

--silly version first
isPrimeHelper :: Int -> Int -> Bool
isPrimeHelper test num 
 | test == 0 = error "1 is not a prime"
 | test == 1 = True
 | mod num test == 0 = False
 | otherwise = isPrimeHelper (test-1) num

isPrime :: Int -> Bool
isPrime n = if n>1 then isPrimeHelper (n-1) n else False

-- better version

notDivisiblePredicate test num = if (mod num test == 0) then False else True

sievePrimeHelper num (x:rest)
 | (x == num) = True
 | (isIn num cut) = sievePrimeHelper num cut 
 | otherwise = False
 where
  cut=(filterer (notDivisiblePredicate x) (x:rest))

sievePrime :: Int -> Bool
sievePrime num = sievePrimeHelper num [2..num]


--Theorem 32

gcder :: Int -> Int -> Int
gcder a b 
 | b>a = gcder b a 
 | mod a b == 0 = b
 | otherwise = (gcder b (mod a b))

-- Theorem 33

isCoPrime :: Int -> Int -> Bool
isCoPrime a b = (gcd a b) == 1

-- Theorem 34

eulerPhi :: Int -> Int
eulerPhi num = longer (filterer (isCoPrime num)[1..num])

-- Theorem 35

isPrimeFactor :: Int -> Int -> Bool
isPrimeFactor num test = (isPrime test) && (mod num test == 0) 

primeFactors :: Int -> [Int]
primeFactors num = filterer (isPrimeFactor num) [1..num]

primeFactorization :: Int -> [Int]
primeFactorization 1 = []
primeFactorization n = firstprime:(primeFactorization (div n firstprime))
 where
  (firstprime:restprimes) = (primeFactors n)

-- Theorem 36

primeFactorizerHelper 1 list lastprime = list
primeFactorizerHelper num list lastprime  
 | firstprime /= lastprime = primeFactorizerHelper (div num firstprime) ((firstprime,1):list) firstprime
 | otherwise = primeFactorizerHelper (div num firstprime) ((firstprime,index+1):rest) firstprime
 where
  (firstprime:restofprimes) = primeFactors num
  (x:rest) = list
  (primething,index) = x


primeFactorizer :: Int -> [(Int,Int)]
primeFactorizer num = rev (primeFactorizerHelper num [] 1)


-- Theorem 37

betterEulerPhiHelper :: [(Int,Int)]->Int
betterEulerPhiHelper list
 | empty list = 1
 | otherwise = (thisprime - 1)*(thisprime^(thispower - 1))*(betterEulerPhiHelper rest)
 where
  (x:rest) = list
  (thisprime,thispower) = x



betterEulerPhi :: Int -> Int
betterEulerPhi num = betterEulerPhiHelper (primeFactorizer num)

-- Theorem 38

-- Try to generate reductions and compare.

-- Theorem 39

primesInRange start stop = filterer isPrime [start..stop]

-- Theorem 40 

goldbachWitnessHelper :: Int -> [Int] -> (Int,Int)
goldbachWitnessHelper num (x:rest)
 | empty (x:rest) = error "Goldbach's conjecture is false!"
 | isPrime (num - x) = (x,num-x)
 | otherwise = goldbachWitnessHelper num rest

goldbachWitness :: Int -> (Int,Int)
goldbachWitness num = if ((mod num 2 == 0) && num >2) then goldbachWitnessHelper num (primesInRange 2 num) else error "Not an even number or number is equal to 2!"

-- Theorem 41

evenAndBiggerThanTwo :: Int -> Bool
evenAndBiggerThanTwo num = if ((mod num 2 == 0) && num >2) then True else False

goldbachRangeHelper :: [Int] -> [(Int,Int,Int)]
goldbachRangeHelper list
 | empty list = []
 | otherwise = (x,firstprime,secondprime):(goldbachRangeHelper rest)
 where
  (x:rest) = list
  (firstprime, secondprime) = (goldbachWitness x)


goldbachRange :: Int -> Int -> [(Int, Int, Int)]
goldbachRange start stop = goldbachRangeHelper (filter evenAndBiggerThanTwo [start..stop])

goldbachBothLargePrimes :: (Int,Int,Int) -> Bool
goldbachBothLargePrimes (num, firstprime, secondprime) = if (firstprime>50 && secondprime>50) then True else False


goldbachRangeLargePrimes :: Int->Int->[(Int,Int,Int)]
goldbachRangeLargePrimes start stop = filter goldbachBothLargePrimes (goldbachRange start stop)

-- there are longer (goldbachRangeLargePrimes 2 3000) many cases = 10 cases.

-- Theorem 46 

data Truth = T | F
 deriving (Show, Eq)


noter::Truth->Truth
noter F=T
noter T=F

flipper :: (Truth->Truth->Truth)->(Truth->Truth->Truth)
(flipper predicate) first second = noter (predicate first second)


ander:: Truth -> Truth -> Truth
ander T T = T
ander _ _ = F

orer :: Truth->Truth->Truth
orer F F = F
orer _ _ = T

nander :: Truth->Truth->Truth
nander T T = F
nander _ _ = T

norer :: Truth->Truth->Truth
norer F F = T
norer _ _ = F

xorer :: Truth->Truth->Truth
xorer first second = if (first==second) then F else T

impler :: Truth->Truth->Truth
impler T F = F
impler _ _ = T

assignments = [T,F]

equiver::(Truth->Truth->Truth)->(Truth->Truth->Truth)->Truth
equiver firstPredicate secondPredicate = if [firstPredicate first second | first<-assignments, second<-assignments] == [secondPredicate first second | first<-assignments, second<-assignments] then T else F

truthTable :: (Truth->Truth->Truth)->[(Truth,Truth,Truth)]
truthTable predicate = [(first,second, predicate first second)| first <- assignments, second<- assignments]

-- Theorem 47

infixander :: Truth -> Truth -> Truth
first `infixander` second = ander first second


-- Theorem 48

-- First I want to figure out how to make all possible truth assignments to the elements of a list.

assigning level
 | level == 1 = [value:[]|value<-assignments]
 | otherwise = [value:thing|value<-assignments,thing<-(assigning (level-1))]

truthTableVariable :: ([Truth]->Truth)->[a]->[[Truth]]
truthTableVariable predicate string = [listadder assigned [(predicate assigned)] | assigned<-(assigning (longer string))]
 
-- Theorem 49

grayCode :: Int -> [[Char]]
grayCode 1 = ["0","1"]
grayCode n = listadder (mapper (listadder "0") (grayCode (n-1))) (mapper (listadder "1") (rev (grayCode (n-1))))


-- Theorem 50

data Tree a = Empty | Node a (Tree a)  (Tree a)
 deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node x Empty Empty

-- Learning a few things about Huffman codes
-- First, sort the table in increasing order of frequency. Then follow siggraph tutorial

-- test list
thing = [("a",45),("b",13),("c",12),("d",16),("e",9),("f",5)]

hufInsert :: (String,Integer)->[(String,Integer)]->[(String,Integer)]
hufInsert entry [] = entry:[]
hufInsert entry (firstlist:restlist)
 | (secondcoordinate entry)>(secondcoordinate firstlist) = listadder (firstlist:[]) (hufInsert entry restlist)
 | otherwise = entry:(firstlist:restlist)

hufSorter::[(String,Integer)]->[(String,Integer)]
hufSorter metalist = rfolder function accumulator metalist
 where
  function list metalist = hufInsert list metalist
  accumulator = []

-- Fine, now let's build a Huffman tree

hufCombiner :: (String, Integer) -> (String, Integer) -> (String, Integer)
hufCombiner first second = ((listadder (firstcoordinate first) (firstcoordinate second)), (secondcoordinate first) + (secondcoordinate second))

hufListTraversalHelper :: ([(String,Integer)],[(String, Integer)]) -> ([(String,Integer)],[(String,Integer)])
hufListTraversalHelper ([],list)  = ([],list)
hufListTraversalHelper ((x:[]),list)  = (x:[],(x:list))
hufListTraversalHelper ((x:y:rest),list) = hufListTraversalHelper ((hufSorter ((hufCombiner x y):rest)),listadder ((hufCombiner x y):rest) (listadder [("",9999)] list))

hufListTraversal :: [(String,Integer)]->([(String,Integer)],[(String,Integer)])
hufListTraversal list = (hufListTraversalHelper ((hufSorter list),(hufSorter list)))

-- Here I want to build a function that extracts the root of a tree

findRoot :: Tree a -> a
findRoot tree = case tree of
                     (Node label _ _) -> label
                     Empty -> error "Empty tree!"

isXInTrees :: (String, Integer)->[Tree (String,Integer)]->Bool
isXInTrees x trees= if isIn x (mapper findRoot trees) then True else False

isYInTrees :: (String, Integer)->[Tree (String,Integer)]->Bool
isYInTrees y trees= if isIn y (mapper findRoot trees) then True else False

--updateTrees (String,Integer)->(String,Integer)->[Tree (String,Integer)]->[Tree (String,Integer)]
--updateTrees x y trees 

 

--hufTreeHelper :: ([(String,Integer)], [Tree (String,Integer)]) -> ([(String,Integer)] ,[Tree (String,Integer)])
--hufTreeHelper (x:[], trees) = (x:[],trees)
--hufTreeHelper (x:y:rest, trees) = ((hufSorter (hufCombiner x y):rest), )




-- Theorem 54A

-- Predicate?

-- Let's try and compute a depth function for trees, just for kicks

depth :: Tree a -> Int
depth Empty = 1
depth (Node _ firsttree secondtree) = 1+ (max (depth firsttree) (depth secondtree))

-- Let's try and compute a number of successors function for each node

nodes :: Tree a -> Int
nodes Empty = 0
nodes (Node _ firsttree secondtree) = 1 + (nodes firsttree) + (nodes secondtree)

-- Theorem 55

{- we are first going to write a cbt function to generate one (canonical) cbt tree with n nodes. after this we'll look into enumerating the entire list via symmetries.-}

upperhelper :: Int -> Int -> Int
upperhelper _ 0 = 0
upperhelper _ 1 = 1
upperhelper l n = if (((2*l) - 1) >= n-1) then l else  (upperhelper (l+1) n)

upper :: Int -> Int
upper n = upperhelper 0 n

lowerhelper :: Int -> Int -> Int
lowerhelper _ n = n - (upperhelper 0 n)

lower :: Int -> Int
lower n = lowerhelper 0 n


cbt :: Int -> [Tree Char]
cbt 0 = [Empty]
cbt 1 = [leaf 'x']
cbt n = if (upper (n-1) /= lower (n-1)) then [Node 'x' left right | index <- [upper (n-1), lower (n-1)], left <- cbt index, right <- cbt (n-1-index)] else [Node 'x' left right | index <- [upper (n-1)], left <- cbt index, right <- cbt (n-1-index)]



-- Theorem 56

revTree :: Tree a -> Tree a
revTree Empty = Empty
revTree (Node x left right) = Node x (revTree right) (revTree left)

symTree :: Eq a => Tree a -> Bool
symTree tree = ((revTree tree) == tree)

-- Theorem 57

bstinserter :: Ord a => Tree a -> a -> Tree a
bstinserter Empty x = Node x Empty Empty
bstinserter (Node y left right) x
 | x<y = Node y (bstinserter left x) right
 | x>y = Node y left (bstinserter right x)
 | x==y = Node x left right


bst :: (Ord a) => [a] -> Tree a
bst list = lfolder bstinserter Empty list

--Theorem 58

symcbt :: Int -> [Tree Char]
symcbt n = filterer (symTree) (cbt n)



--Theorem 59

--We construct hbts of height N recursively by joining a root to any of: two hbts of height N-1, one hbt of height N-1 and one hbt of height N-2, one hbt of height N-2 and one hbt of height N-1.

hbtlist:: Int -> [Tree Char]
hbtlist (-1) = [Empty]
hbtlist 0 = [Node 'x' Empty Empty]
hbtlist n = listadder (listadder ([Node 'x' left right | left<- hbtlist (n-1), right <- hbtlist (n-1)]) ([Node 'x' left right | left<- hbtlist (n-1), right <- hbtlist (n-2)])) ([Node 'x' left right | left<- hbtlist (n-2), right <- hbtlist (n-1)])


--Theorem 60

--First, we need to prove that every hbt tree of a given height with the minimum possible number of nodes is equivalent under certain symmetries to a tree of a canonical form (left justified). Then, we prove that the recursive whisker construction is optimal. Then we use the recursion below:

minhbt:: Int -> Int
minhbt 0 = 1
minhbt 1 = 2
minhbt n = (minhbt (n-1)) + 1 + (minhbt (n-2))




--Theorem 61

leaves :: Tree a -> Int
leaves Empty = 0
leaves (Node x Empty Empty) = 1
leaves (Node x left right) = (leaves left) + (leaves right)

--Theorem 61A

leafRaker :: Tree a -> [a]
leafRaker Empty = []
leafRaker (Node x Empty Empty) = [x]
leafRaker (Node x left right) = listadder (leafRaker left) (leafRaker right)


--Theorem 62

nodelist :: Tree a -> [a]
nodelist Empty = []
nodelist (Node x left right) = x:(listadder (nodelist left) (nodelist right))

internalNodes :: Tree a -> [a]
internalNodes (Node x Empty Empty) = []
internalNodes (Node x left right) = x:(listadder (internalNodes left) (internalNodes right))



reduced :: Eq a => [a] -> [a]
reduced [] = []
reduced (x:xs) = if (isIn x xs) then xs else (x:(reduced xs))


