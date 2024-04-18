{-
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
-}

module IntroSpickzettel where

import Prelude hiding (Foldable, Functor, Semigroup, Monoid)

-- Basistypen in Haskell
-- Bool: False, True
-- Char, z.B. 'a', 'A', '3', '_', '\n', '\t'
-- String, z.B. "abc", "1+2+3"
-- Int, z.B. -100, 99, 0
-- Float, z.B. -12.34, 1.0, 3.1415927
-- Tupel (T1, ..., Tn), () leerer Tupel (Sequenz von Komponenten, die je unterschiedliche Typen annehmen können)
-- Listen (später, Unterschied zu Tupeln: innerhalb einer Liste besteht Konsistenz bzgl. des Typs)
-- es gibt auch Integer und Double 

-- Konstanten
z :: Integer
z = 3

y :: Bool
y = True

x :: String
x = "Erika"

-- Funktionen
-- Zahl verdoppeln
double :: Int -> Int
double x = x * 2
-- double 2 = 2 * 2 = 4 
-- \x -> x * 2, Lambdaschreibweise

-- Zwei Zahlen addieren
add :: Int -> Int -> Int
add x y = x + y
-- add 5 7 = 12

-- curried functions, Typinferenz

increment :: Int -> Int
increment = add 1
-- increment y = add 1 y = 1 + y
-- increment 5 = add 1 5 = 6

-- Ein Aggregatzustand ist eins der folgenden:
-- - gasförmig
-- - flüssig
-- - fest
data State = Gas | Liquid | Solid
    --deriving Show
    

-- data GanzeZahlen = ... -1 | 0 | 1 ...
-- data Bool = True | False

-- Agreggatzustand von Wasser, guards
computeState :: Float -> State
computeState t
    | t < 0 = Solid
    | (t >= 0) && (t <= 100) = Liquid
    | otherwise = Gas
-- computeState 75

-- Typische Temperatur, pattern matching
typicalTemp :: State -> Float
typicalTemp Gas = 150
typicalTemp Liquid = 20
typicalTemp Solid = -20

-- Brauchen Datendefinition für Lebendigkeit:
-- Lebendigkeit kann folgende Werte annehmen:
-- - tot
-- - lebendig
data Liveness = Dead | Alive
 deriving (Show, Eq)

-- Zusammengesetzte Daten
-- Ein Gürteltier hat die folgenden Eigenschaften:
-- - Lebendigkeit
-- - Gewicht
data Dillo = Dillo {dilloLiveness :: Liveness,
                    dilloWeight :: Int}
    deriving (Show, Eq)

-- Beispiele
dillo1 :: Dillo
dillo1 = Dillo Alive 10 -- lebendiges Gürteltier, 10kg
dillo2 :: Dillo
dillo2 = Dillo Dead 12 -- totes Gürteltier, 12kg

-- Beispiele
-- dilloLiveness dillo1
-- dilloLiveness dillo2
-- dilloWeight dillo1

-- Gürteltier füttern, wildcard pattern
feedDillo :: Int -> Dillo -> Dillo
feedDillo amount (Dillo Alive weight) = Dillo Alive (weight + amount)
feedDillo _amount dillo@(Dillo Dead _) = dillo

-- Beispiele
-- feedDillo 1 dillo1
-- feedDillo 1 dillo2

-- Noch ein Beispiel für zusammengesetzte Daten
-- Ein Papagei hat folgende Eigenschaften
-- - Satz, den er sagt
-- - Gewicht
data Parrot = Parrot {parrotSentence :: String,
                      parrotWeight :: Int}
    deriving (Show, Eq)

-- Beispiele
parrot1 :: Parrot
parrot1 = Parrot "Der Schatz ist im Silbersee!" 5 -- Piraten-Papagei, 5kg
parrot2 :: Parrot
parrot2 = Parrot "Ciao" 6 -- Haus-Papagei, 6kg

-- Papagei füttern
feedParrot :: Int -> Parrot -> Parrot
feedParrot amount (Parrot sentence weight) = Parrot sentence (weight + amount)

-- Beispiel
-- feedParrot 1 parrot1 

{- 
-- Algebraischer Datentyp: Gemischte Daten von zusammengesetzten Daten
-- Ein Tier ist entweder ein Dillo oder ein Papagei:
data Animal = Dillo { dilloLiveness :: Liveness,
                      dilloWeight :: Int }
            | Parrot { parrotSentence :: String,
                       parrotWeight :: Int }
 deriving (Show, Eq)

-- Beispiele
dillo1 :: Animal
dillo1 = Dillo Alive 10 -- lebendiges Gürteltier, 10kg
dillo2 :: Animal
dillo2 = Dillo Dead 12 -- totes Gürteltier, 12 kg
parrot1 :: Animal
parrot1 = Parrot "Der Schatz ist am Silbersee" 5 -- Piraten-Papagei, 5kg

-- Tiere füttern
feedAnimal :: Int -> Animal -> Animal

feedAnimal amount (Dillo Alive weight) = Dillo Alive (weight + amount)

feedAnimal _amount dillo@(Dillo Dead _) = dillo

feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)

-- Beispiele
-- feedAnimal 1 dillo1
-- feedAnimal 2 dillo2
-- feedAnimal 1 parrot1
-}

-- Listen, parametrisierter Typkonstruktor, rekursiver Datentyp, gemischte Daten

-- Beispiele
list0 :: [a]
list0 = [] -- leere Liste

list1 :: [Int]
list1 = 5 : [] -- 1 elementige Liste: [5] -- (:) fügt ein Element vorne an die Liste an

list2 :: [Int]
list2 = 7 : [5] -- = 7 : 5 : [] -- 2-elementige Liste: [5, 7]

list3 :: [Int]
list3 = 12 : [5,7] -- = 12 : 7 : 5 : [] -- 3-elementige Liste: [12, 5, 7]

list4 :: [Bool]
list4 = [False, True, False] -- 3-elementige Liste: False, True, False

list5 :: [Char] -- = String 
list5 = ['a', 'b', 'c', 'd'] -- = "abcd" -- 4-elementige Liste: [a, b, c, d]

list6 :: [String] -- list 6 :: [[Char]]
list6 = ["ab", "cde", "fghij"] -- = [['a', 'b'], ['c', 'd', 'e'], ['f', 'g', 'h', 'i', 'j']] -- 3-elementige Liste: ["ab", "cde", "fghij"]

-- Funktionen auf Listen:
x1 :: Int
x1 = sum [1, 2, 3, 4, 5] -- 15, Addiere Einträge in einer Liste auf

x2 :: Int
x2 =  head [1, 2, 3, 4, 5] -- 1, erster Eintrag aus der Liste

list8 :: [Int]
list8 = tail [1, 2, 3, 4 ,5] -- [2, 3, 4 ,5], Liste ohne den ersten Eintrag

x3 :: Int
x3 = [1, 2, 3, 4, 5] !! 2 -- 3, dritter Eintrag aus der Liste

list9 :: [Int]
list9 = take 3 [1, 2, 3, 4, 5] -- [1, 2, 3], die ersten drei Einträge aus der Liste

list10 :: [Int]
list10 = drop 3 [1, 2, 3, 4, 5] -- [4, 5], Liste ohne die ersten drei Einträge

x4 :: Int
x4 = length [1, 2, 3, 4, 5] -- 5, Anzahl der Einträge in einer Liste

x5 :: Int
x5 = product [1, 2, 3, 4, 5] -- 120, multipliziere Einträge in einer Liste auf

list11 :: [Int]
list11 = [1, 2, 3] ++ [4, 5] -- [1, 2, 3, 4, 5], verbinde zwei Listen

list12 :: [Int]
list12 = reverse [1, 2, 3, 4, 5] -- [5, 4, 3, 2, 1], drehe die Reihenfolge der Einträge in einer Liste um

-- Rekursion auf Listen:
-- Addiere Einträge in einer Liste auf
-- also sowas: listSum [1,2,3,4,5] = 15
listSum :: [Int] -> Int
listSum [] = 0
listSum (first:rest) = first + (listSum rest)

-- listSum [1, 2, 3, 4, 5] -- 15
-- Zwischenschritte:
-- listSum [1, 2, 3, 4, 5]
--                                                {apply listSum}
-- 1 + listSum [2, 3, 4, 5]
--                                                {apply listSum}
-- 1 + 2 + listSum [3, 4, 5]
--                                                {apply listSum}
-- 1 + 2 + 3 + listSum [4, 5]
--                                                {apply listSum}
-- 1 + 2 + 3 + 4 + listSum [5]
--                                                {apply listSum}
-- 1 + 2 + 3 + 4 + 5 + listSum []
--                                                {apply listSum}
-- 1 + 2 + 3 + 4 + 5 + 0

-- Eingebaut: sum

-- Zwei Listen zippen:
-- also sowas: zipLists ['a', 'b', 'c'] [1, 2, 3, 4] = [('a', 1), ('b', 2), ('c', 3)]  
zipLists :: [a] -> [b] -> [(a, b)]
zipLists [] _ = []
zipLists _ [] = []
zipLists (x:xs) (y:ys) = (x,y) : zipLists xs ys

-- zipLists ['a', 'b', 'c'] [1, 2, 3, 4] -- [('a', 1), ('b', 2), ('c', 3)]
-- Zwischenschritte:
-- zipLists ['a', 'b', 'c'] [1, 2, 3, 4]
--                                                {apply zipLists}
-- ('a', 1) : (zipLists ['b', 'c'] [2, 3, 4])
--                                                {apply zipLists}
-- ('a', 1) : (('b', 2) : (zipLists ['c'] [3, 4]))
--                                                {apply zipLists}
-- ('a', 1) : (('b', 2) : (('c', 3) : (zipLists [] [4])))
--                                                {apply zipLists}
-- ('a', 1) : (('b', 2) : (('c', 3) : []))
-- [('a', 1), ('b', 2), ('c', 3)]

-- Eingebaut: zip

-- Mehrfache Rekursion:
-- quicksort:
-- also sowas: qsort [9,18,81,90,72] = [9, 18, 72, 81, 90]
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger = [b | b <- xs, b > x]

-- qsort [9, 18, 81, 27, 36, 90, 72, 45, 63, 54] -- [9, 18, 27, 36, 45, 54, 63, 72, 81, 90]
-- Zwischenschritte:
-- qsort [9, 18, 81, 27, 36, 90, 72, 45, 63, 54]
--                                                 {apply qsort}
-- (qsort []) ++ [9] ++ (qsort [18, 81, 27, 36, 90, 72, 45, 63, 54])
--                                                 {apply qsort}
-- [] ++ [9] ++ (qsort []) ++ [18] ++ (qsort [81, 27, 36, 90, 72, 45, 63, 54])
--                                                 {apply qsort}
-- [] ++ [9] ++ [] ++ [18] ++ (qsort [27, 36, 72, 45, 63, 54]) ++ [81] ++ (qsort [90])
--                                                 {apply qsort}
-- [] ++ [9] ++ [] ++ [18] ++ (qsort []) ++ [27] ++ (qsort [36, 72, 45, 63, 54]) ++ [81] ++ (qsort []) ++ [90] ++ (qsort [])
--                                                 {apply qsort}
-- [] ++ [9] ++ [] ++ [18] ++ [] ++ [27] ++ (qsort []) ++ [36] ++ (qsort [72, 45, 63, 54]) ++ [81] ++ [] ++ [90] ++ []
--                                                 {apply qsort}
-- [] ++ [9] ++ [] ++ [18] ++ [] ++ [27] ++ [] ++ [36] ++ (qsort [45, 63, 54]) ++ [72] ++ (qsort []) ++ [81] ++ [] ++ [90] ++ []
--                                                 {apply qsort}
-- [] ++ [9] ++ [] ++ [18] ++ [] ++ [27] ++ [] ++ [36] ++ (qsort []) ++ [45] ++ (qsort [63, 54]) ++ [72] ++ [] ++ [81] ++ [] ++ [90] ++ []
--                                                 {apply qsort}
-- [] ++ [9] ++ [] ++ [18] ++ [] ++ [27] ++ [] ++ [36] ++ [] ++ [45] ++ (qsort [54]) ++ [63] ++ (qsort []) ++ [72] ++ [] ++ [81] ++ [] ++ [90] ++ []
--                                                 {apply qsort}
-- [] ++ [9] ++ [] ++ [18] ++ [] ++ [27] ++ [] ++ [36] ++ [] ++ [45] ++ (qsort []) ++ [54] ++ (qsort []) ++ [63] ++ [] ++ [72] ++ [] ++ [81] ++ [] ++ [90] ++ []
--                                                 {apply qsort}
-- [] ++ [9] ++ [] ++ [18] ++ [] ++ [27] ++ [] ++ [36] ++ [] ++ [45] ++ [] ++ [54] ++ [] ++ [63] ++ [] ++ [72] ++ [] ++ [81] ++ [] ++ [90] ++ []
-- [9, 18, 27, 36, 45, 54, 63, 73, 81, 90]

-- Wechselnde Rekursion:
-- Funktion, die alle Elemente an geraden Stellen aus einer Liste nimmt:
-- also sowas: evens "abcde" = ...
evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

-- Funktion, die alle Elemente an ungeraden Stellen aus einer Liste nimmt:
-- also sowas: odds "bcde" = ...
odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

-- evens "abcde" --"ace"
-- Zwischenschritte:
-- evens "abcde"
--                                                 {apply evens}
-- 'a' : (odds "bcde")
--                                                 {apply odds}
-- 'a' : (evens "cde")
--                                                 {apply evens}
-- 'a' : ('c' : (odds "de"))
--                                                 {apply odds}
-- 'a' : ('c' : (evens 'e'))
--                                                 {apply evens}
-- 'a' : ('c' : ('e' : (odds [])))
--                                                 {apply odds}
-- 'a' : ('c' : ('e' : []))
-- "ace"

-- Funktionen höherer Ordnung auf Listen:

-- Eine Funktion auf jeden Eintrag einer Liste anwenden: 
-- Beispiel: Bestimme die ersten 5 ungeraden Zahlen
-- also sowas: listMap ... = ...
listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x:xs) = (f x) : (listMap f xs)

-- Eingebaut: map

-- listMap (\x -> x * 2 + 1) [0, 1, 2, 3, 4] -- [1, 3, 5, 7, 9]
-- Zwischenschritte:
-- listMap (\x -> x * 2 + 1) [0, 1, 2, 3, 4]
--                                                 {apply listMap}
-- 1 : (listMap (\x -> x * 2 + 1) [1, 2, 3, 4])
--                                                 {apply listMap}
-- 1 : (3 : (listMap (\x -> x * 2 + 1) [2, 3, 4]))
--                                                 {apply listMap}
-- 1 : (3 : (5 : (listMap (\x -> x * 2 + 1) [3, 4])))
--                                                 {apply listMap}
-- 1 : (3 : (5 : (7 : (listMap (\x -> x * 2 + 1) [4]))))
--                                                 {apply listMap}
-- 1 : (3 : (5 : (7 : (9 : (listMap (\x -> x * 2 + 1) [])))))
--                                                 {apply listMap}
-- 1 : (3 : (5 : (7 : (9 : []))))
-- [1, 3, 5, 7, 9]



-- Die Einträge einer Liste miteinander verknüpfen, am Ende der Liste angefangen:
-- Beispiel: andere Möglichkeit für sum:
-- also sowas: sum' [1,2,3,4,5] = 15
sum' :: [Int] -> Int
sum' xs = listFoldr (+) 0 xs

listFoldr :: (a -> b -> b) -> b -> [a] -> b
listFoldr f v [] = v
listFoldr f v (x:xs) = f x (listFoldr f v xs)

-- Eingebaut: foldr

-- sum' [1, 2, 3, 4, 5] -- 15
-- Zwischenschritte:
-- sum' [1, 2, 3, 4, 5]
--                                                   {apply sum'}
-- listFoldr (+) 0 [1, 2, 3, 4, 5]
--                                                   {apply listFoldr}
-- (+) 1 (listFoldr (+) 0 [2, 3, 4, 5])
--                                                   {apply listFoldr}
-- (+) 1 ((+) 2 (listFoldr (+) 0 [3, 4, 5]))
--                                                   {apply listFoldr}
-- (+) 1 ((+) 2 ((+) 3 (listFoldr (+) 0 [4, 5])))
--                                                   {apply listFoldr}
-- (+) 1 ((+) 2 ((+) 3 ((+) 4 (listFoldr (+) 0 [5]))))
--                                                   {apply listFoldr}
-- (+) 1 ((+) 2 ((+) 3 ((+) 4 ((+) 5 (listFoldr (+) 0 [])))))
--                                                   {apply listFoldr}
-- (+) 1 ((+) 2 ((+) 3 ((+) 4 ((+) 5 0))))
-- 15

-- Weitere Beispiele: 

-- Listenelemente aufmultiplizieren

-- Listen in einer Liste konkatenieren / zusammenkleben

-- Eine Liste von Funktionen a -> a verketten

{-
-- Funktoren, Foldables

-- Optional, manchmal da, manchmal nicht:
data Optional a =
    NotThere
  | There a
 deriving Show

-- Beispiel
opt1 :: Optional String
opt1 = There "Schokoladensahnetorte"

opt2 :: Optional String
opt2 = NotThere

-- Eingebaut: data Maybe a = Nothing | Just a

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f NotThere  = NotThere
optionalMap f (There a) = There (f a)

-- Beispiel
opt3 :: Optional String
opt3 = optionalMap (++ " mit Sahne") opt1 -- There "Schokoladensahnetorte mit Sahne"

opt4 :: Optional String
opt4 = optionalMap (++ " mit Sahne") opt2 -- NotThere

-- Funktor, z.B. [], Optional
class Functor (constructor :: * -> *) where
    mmap :: (a -> b) -> constructor a -> constructor b
    -- mit folgenden Funktoreigenschaften:
    -- mmap identity_a = identity_constructor a
    -- mmap (f . g) = (mmap f) . (mmap g)

-- Erläuterungen:
-- identity_a :: a -> a ist die Identität auf a und entsprechend hat man mmap identity_a :: constructor a -> constructor a.
-- Die erste Funktoreigenschaft besagt, dass mmap identity_a der Identität auf constructor a,
-- also identity_constructor a :: constructor a -> constructor a entsprechen muss.

-- Für g :: a -> b und f :: b -> c hat man f . g :: a -> c und entsprechend mmap (f . g) :: constructor a -> constructor c.
-- Auf der anderen seite hat man mmap g :: constructor a -> constructor b und mmap f :: constructor b -> constructor c
-- und entsprechend (mmap f . mmap g) :: constructor a -> constructor c.
-- Die zweite Funktoreigenschaft besagt, dass die Abbildung mmap (f . g) der Abbildung (mmap f . mmap g) entsprechend muss.

-- Der Typkonstruktor [] ist mit der Abbildung listMap ein Funktor
--instance Functor [] where
    --mmap = listMap

-- Der Typkonstruktor Optional ist mit der Abbildung optionalMap ein Funktor
--instance Functor Optional where
    --mmap = optionalMap

-- Foldable
class Foldable (constructor :: * -> *) where
    myfoldr :: (a -> b -> b) -> b -> constructor a -> b

-- Der Typkonstruktor [] ist mit der Abbildung listFoldr ein Foldable
--instance Foldable [] where
    --myfoldr = listFoldr

optionalFoldr :: (a -> b -> b) -> b -> Optional a -> b
optionalFoldr f v NotThere = v
optionalFoldr f v (There a) = f a v

-- Der Typkonstruktor Optional ist mit der Abbildung optionalFoldr ein Foldable
instance Foldable Optional where
    myfoldr = optionalFoldr
-}





