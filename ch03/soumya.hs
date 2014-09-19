type Name = String
type Age = Int
type Breed = String
type Color = String

data Doggy = Dog Name Breed Age Color
 deriving Show

data Who = Person Name Age
 deriving Show

data Soumya = Who | Doggy
 deriving Show

data A = B Int | C String
 deriving Show

data D = E Int | F String | D
 deriving Show

data HouseholdMember = Human Name Age | Canine Name Breed Age
 deriving (Show, Eq)

data Cart = Cartesian Double Double
 deriving (Eq, Show)

data Polar = Polar Double Double
 deriving (Eq, Show)

data Roygbiv = Red
 | Orange
 | Yellow
 | Green
 | Blue
 | Indigo
 | Violet
 deriving (Eq, Show)

myNot True = False
myNot False = True

sumList (x:xs) = x + sumList xs
sumList [] = 0

hname (Human name age) = name
hage (Human name age) = age

cname (Canine name _ _ ) =  name
cage (Canine _ _ age) = age
cbreed (Canine _ breed _) = breed

data Expert = Expert {
 name :: String,
 fields::[String],
 degrees::[String]
 } deriving (Show, Eq)

data List a = Cons a (List a) | Nil
 deriving (Show, Eq)

data Tree a = Node a (Tree a) (Tree a) | Empty
 deriving (Show, Eq)

fromList (x:xs) = Cons x (fromList xs)
fromList []=Nil

toList Nil = []
toList (Cons x y) = x: (toList y)

what::[a]->a
what xs = if null (tail xs)
 then error "Too short!"
 else head (tail xs)

safe::[a] -> Maybe a
safe [] = Nothing
safe xs = if null (tail xs)
 then Nothing
 else Just (head (tail xs))

tidy::[a]-> Maybe a
tidy (_:x:_) = Just x
tidy _ = Nothing

lend amount balance = let 
 reserve = 100
 newBalance = balance - amount
 in 
 if balance < reserve
 then Nothing
 else Just newBalance

foo = 
 let
 a = 1
 in
 let b = 2
 in
 a+b

bar = let
 x=1
 in
 ((let x = "foo" in x),x)

quux a = 
 let
  a="foo"
 in
 a++"eek!"

lending amount balance = 
 if amount < reserve*0.5
 then Just newBalance
 else Nothing
 where
  reserve=100
  newBalance=balance-amount

pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
 where 
  plural 0 = "no "++word++"s"
  plural 1 = "one "++word
  plural n = show n ++ " " ++ word ++ "s"

hee =
 let 
  b=2
  c=True
 in
  let a=b
   in (a,c)

haa = x
 where 
  x=y
   where 
    y=2

unwrap trash thing =
 case thing of
  Nothing -> trash
  Just bleh -> bleh

data Fruit = Apple | Banana
 deriving (Show, Eq)

whichFruit::String -> Fruit
whichFruit f =
 case f of
  "apple" -> Apple
  "banana" -> Banana

same (Node a _ _ ) (Node b _ _)
 | a==b = Just a
same _ _ = Nothing

lender amount balance
 | amount<=0 = Nothing
 | amount > reserve*0.5 = Nothing
 | otherwise = Just newBalance
 where
  reserve = 100 
  newBalance = balance - amount

myDrop n xs = 
 if n<=0 || null xs
 then xs
 else myDrop (n-1) (tail xs)

dropper n xs
 | n<=0 || (null xs) = xs
 | otherwise = dropper (n-1) (tail xs)

longer :: [a] -> Int
longer xs 
 | (null xs) = 0
 | otherwise = 1 + (longer (tail xs))

palindrome:: (Eq a) => [a] -> [a] 
palindrome string
 | (head string == last string) = 
  let 
   bit = reverse ( tail (reverse string))
   point = [last string]
  in
   bit ++ point ++ (reverse bit)
 | otherwise = string ++ (reverse string)

ifpal :: (Eq a)=> [a] -> Bool
ifpal string
 | string == (reverse string) = True
 | otherwise = False

splitLines [] = []
splitLines cs = 
 let (pre,suf) = break isLineTerminator cs
 in pre : case suf of
               ('\r':'\n':rest) -> splitLines rest
               ('\r':rest) -> splitLines rest
               ('\n':rest) -> splitLines rest
               _ -> []

isLineTerminator c = (c == '\r' || c == '\n')

a `plus` b = a+b

data a `Pair` b = a `Pair` b
 deriving (Show)

-- Here we're trying to write standard list functions from scratch

long::[a] -> Int
long [] = 0
long (x:xs) = 1+long(xs)

empty :: [a] -> Bool
empty [] = True
empty _ = False

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

listadder :: [a] -> [a] -> [a]
listadder lista listb
 | empty lista = listb
 | otherwise = x: (listadder rest listb)
 where
  (x:rest) = lista

cat :: [[a]] -> [a]
cat list
 | empty list = []
 | otherwise = listadder x (cat rest)
 where
  (x:rest) = list

rev:: [a] -> [a]
rev [] = []
rev (x:xs) = listadder (rev xs) [x]

final :: [a] -> a
final list = top (rev list)

finaller :: [a] -> a
finaller list
 | empty list = error "Empty list, silly!"
 | (not (empty xs)) = finaller xs
 | (empty xs) = x
 | otherwise = error "Invalid arguments!"
 where
  (x:xs) = list

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


