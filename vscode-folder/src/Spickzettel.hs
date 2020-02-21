{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Spickzettel where

import Prelude hiding (Foldable, Functor, Semigroup, Monoid)

-- Variablen
z :: Int
z = 3

-- Funktionen
-- Zahl verdoppeln
double :: Int -> Int
double x = x * 2

-- Zwei Zahlen addieren
add :: Int -> Int -> Int
add x y = x + y

-- curried function
-- add' :: Int -> (Int -> Int)
-- (add' x) y = x + y

-- Beispiel:
add' :: Int -> (Int -> Int)
(add' 1) y = 1 + y

-- Ein Aggregatzustand ist eins der folgenden:
-- - gasförmig
-- - flüssig
-- - fest
data State = Gas | Liquid | Solid
 deriving Show

-- Agreggatzustand von Wasser
computeState :: Float -> State
computeState t
    | t < 0 = Solid
    | (t >= 0) && (t <= 100) = Liquid
    | t > 100 = Gas

-- Typsiche Temperatur
-- pattern matching
typicalTemp :: State -> Float
typicalTemp Gas = 150
typicalTemp Liquid = 20
typicalTemp Solid = -20

-- Eine Form ist eins der folgenden:
-- - Kreis
-- - Rechteck
data Shape = Circle Float | Rect Float Float
 deriving (Show, Eq)

-- Wir erstellen ein Quadtrat
square :: Float -> Shape
square n = Rect n n

-- Wir berechnen die Fläche
-- pattern matching
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- Wir berechnen den Umfang
-- pattern matching
circumference :: Shape -> Float
circumference (Circle r) = 2 * pi * r
circumference (Rect x y) = 2 * x + 2 * y

-- Zusammengesetzte Daten
-- Ein Gürteltier hat die folgenden Eigenschaften:
-- - Lebendigkeit
-- - Gewicht
--data Dillo = Dillo Liveness Int
--deriving Show

-- Brauche Datendefinition für Lebendigkeit:
-- Lebendigkeit kann folgende Werte annehmen:
-- - tot
-- - lebendig
data Liveness = Dead | Alive
 deriving (Show, Eq)

-- Beispiele

dillo2 = Dillo Dead 12 -- totes Gürteltier, 12kg

-- Lebt der Dillo?
-- pattern matching
--liveness :: Dillo -> Liveness
--liveness (Dillo Alive _) = Alive
--liveness (Dillo Dead _) = Dead

-- Beispiele
--res1 = liveness dillo1 -- Alive
--res2 = liveness dillo2 -- Dead

-- Gewicht des Dillos?
-- pattern matching
--weight :: Dillo -> Int
--weight (Dillo _ w) = w

-- Beispiel
--res3 = weight dillo1 -- 10
 
-- Besser: Record Syntax
--data Dillo = Dillo {dilloLiveness :: Liveness,
--                    dilloWeight :: Int}
--deriving (Show, Eq)

-- Beispiele
--res4 = dilloLiveness dillo1 -- Alive
--res5 = dilloLiveness dillo2 -- Dead
--res6 = dilloWeight dillo1 -- 10

-- Gürteltier füttern
--feedDillo :: Int -> Dillo -> Dillo
--feedDillo amount (Dillo Alive weight) = Dillo Alive (weight + amount)
--feedDillo amount (Dillo Dead weight) = Dillo Dead weight

-- Beispiele
--dillo3 = feedDillo 1 dillo1
--dillo4 = feedDillo 1 dillo2

-- Gürteltier überfahren
-- pattern matching
--runOverDillo :: Dillo -> Dillo
--runOverDillo (Dillo _ weight) = Dillo Dead weight

-- Beispiele
--dillo5 = runOverDillo dillo3
--dillo6 = runOverDillo dillo2

-- Algebraischer Datentyp: Gemischte Daten von zusammengesetzten Daten
-- Ein Tier ist entweder ein Dillo oder ein Papagei:

data Animal = Dillo { dilloLiveness :: Liveness,
                      dilloWeight :: Int }
            | Parrot { parrotSentence :: String,
                       parrotWeight :: Int }
 deriving Show

-- Beispiele
dillo7 = Dillo Alive 10
dillo8 = Dillo Dead 12
parrot1 = Parrot "Der Schatz ist am Silbersee" 5

-- Tiere füttern
feedAnimal :: Int -> Animal -> Animal
feedAnimal amount (Dillo Alive weight) =
    Dillo Alive (weight + amount)
feedAnimal amount (Dillo Dead weight) =
    Dillo Dead weight
feedAnimal amount (Parrot sentence weight) =
    Parrot sentence (weight + amount)

-- Beispiele
dillo9 = feedAnimal 1 dillo7
dillo10 = feedAnimal 2 dillo8
parrot2 = feedAnimal 1 parrot1

-- Listen ([] ist ein parametrisierter Typkonstruktor):

-- Beispiele:
list1 :: [Int]
list1 = [5] -- 1elementige Liste: 5

list2 :: [Int]
list2 = [5, 7] -- 2elementige Liste: 5, 7

list3 :: [Int]
list3 = 12:list2 -- 3elementige Liste: 12 5 7, : fügt ein Element vorne an die Liste an

list4 :: [Bool]
list4 = [False, True, False] -- 3elementige Liste: False, True, False

list5 :: [Char]
list5 = ['a', 'b', 'c', 'd'] -- 4elementige Liste: a, b, c, d

list6 :: [String]
list6 = ["One", "Two", "Three"] -- 3elementige Liste: One, Two, Three

list7 :: [[Char]]
list7 = [['a', 'b'], ['c', 'd', 'e']] -- 2elementige Liste: ['a', 'b'], ['c', 'd', 'e']

-- Funktionen auf Listen:
x1 = sum [1, 2, 3, 4, 5] -- 15, Addiere Einträge in einer Liste auf
x2 =  head [1, 2, 3, 4, 5] -- 1, erster Eintrag aus der Liste
list8 = tail [1, 2, 3, 4 ,5] -- [2, 3, 4 ,5], Liste ohne den ersten Eintrag
x3 = [1, 2, 3, 4, 5] !! 2 -- 3, dritter Eintrag aus der Liste
list9 = take 3 [1, 2, 3, 4, 5] -- [1, 2, 3], die ersten drei Einträge aus der Liste
list10 = drop 3 [1, 2, 3, 4, 5] -- [4, 5], Liste ohne die ersten drei Einträge
x4 = length [1, 2, 3, 4, 5] -- 5, Anzahl der einträge in einer Liste
x5 = product [1, 2, 3, 4, 5] -- 120, multipliziere Einträge in einer Liste auf
list11 = [1, 2, 3] ++ [4, 5] -- [1, 2, 3, 4, 5], verbinde zwei Listen
list12 = reverse [1, 2, 3, 4, 5] -- [5, 4, 3, 2, 1], drehe die Reihenfolge der Einträge in einer Liste um

-- Spiele mit Funktionen auf Listen:
-- Teile Liste nach dem n-ten Element:
splitListAt :: Int -> [a] -> ([a], [a])
splitListAt n xs = (take n xs, drop n xs)

tuple1 = splitListAt 3 [1, 2, 3, 4, 5] -- ([1, 2, 3], [4, 5])

-- Eingebaut:
-- tuple1 = splitAt 3 [1, 2, 3, 4, 5] -- ([1, 2, 3], [4, 5])

-- Rekursion:
-- Natürliche Zahlen ab n:
natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n + 1)

-- Natürliche Zahlen ab 0:
nats = natsFrom 0
-- Zwischenschritte:
-- natsfrom 0 = 0 : (1 : (2 : (3 : [...])))

-- Rekursion auf Listen:
-- Addiere Einträge in einer Liste auf
listSum :: [Int] -> Int
listSum [] = 0
listSum (first:rest) = first + (listSum rest)

--x1 = listSum [1, 2, 3, 4, 5] -- 15
-- Zwischenschritte:
-- listSum [1, 2, 3, 4, 5]
--                                                {apply listSum}
-- 1 + listSum [2, 3, 4, 5]
--                                                {apply listSum}
-- 1 + 2 listSum [3, 4, 5]
--                                                {apply listSum}
-- 1 + 2 + 3 listSum [4, 5]
--                                                {apply listSum}
-- 1 + 2 + 3 + 4 listSum [5]
--                                                {apply listSum}
-- 1 + 2 + 3 + 4 + 5 listSum []
--                                                {apply listSum}
-- 1 + 2 + 3 + 4 + 5 + 0

-- Vielfache von n aus einer Liste entfernen:
strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n [] = []
-- `rem` -- Funktion rem, nur Infix
strikeMultiples n (first:rest) =
    if (first `rem` n) == 0 -- first ist Vielfaches von n
    then strikeMultiples n rest
    else first : (strikeMultiples n rest)

list13 = strikeMultiples 3 [1, 2, 3, 4, 5, 6, 7, 8, 9] -- [1, 2, 4, 5, 7, 8]
-- Zwischenschritte:
-- strikeMultiples 3 [1, 2, 3, 4, 5, 6, 7, 8, 9]
--                                                {apply strikeMultiples}
-- 1 : (strikeMultiples 3 [2, 3, 4, 5, 6, 7, 8, 9])
--                                                {apply strikeMultiples}
-- 1 : (2 : (strikeMultiples 3 [3, 4, 5, 6, 7, 8, 9]))
--                                                {apply strikeMultiples}
-- 1 : (2 : (strikeMultiples 3 [4, 5, 6, 7, 8, 9]))
--                                                {apply strikeMultiples}
-- 1 : (2 : (4 : (strikeMultiples 3 [5, 6, 7, 8, 9])))
--                                                {apply strikeMultiples}
-- 1 : (2 : (4 : (5 : (strikeMultiples 3 [6, 7, 8, 9]))))
--                                                {apply strikeMultiples}
-- 1 : (2 : (4 : (5 : (strikeMultiples 3 [7, 8, 9]))))
--                                                {apply strikeMultiples}
-- 1 : (2 : (4 : (5 : (7 : (strikeMultiples 3 [8, 9])))))
--                                                {apply strikeMultiples}
-- 1 : (2 : (4 : (5 : (7 : (8 : (strikeMultiples 3 [9]))))))
--                                                {apply strikeMultiples}
-- 1 : (2 : (4 : (5 : (7 : (8 : (strikeMultiples 3 []))))))
--                                                {apply strikeMultiples}
-- 1 : (2 : (4 : (5 : (7 : (8 : [])))))
-- [1, 2, 4, 5, 7, 8]

-- Entferne Vielfache aller Listeneinträge zum Ende der Liste hin:
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (first:rest) = first : (sieve (strikeMultiples first rest))

list14 = sieve [9, 3, 27, 2, 4, 25] -- [9, 3, 2, 25]
-- Zwischenschritte:
-- sieve [9, 3, 27, 2, 4, 25]
--                                                {apply sieve}
-- 9 : (sieve (strikeMultiples 9 [3, 27, 2, 4, 25]))
--                                                {apply strikeMultiples}
-- 9 : (sieve [3, 2, 4, 25])
--                                                {apply sieve}
-- 9 : (3 : (sieve (strikeMultiples 3 [2, 4, 25])))
--                                                {apply strikeMultiples}
-- 9 : (3 : (sieve [2, 4, 25]))
--                                                {apply sieve}
-- 9 : (3 : (2 : (sieve (strikeMultiples 2 [4, 25]))))
--                                                {apply strikeMultiples}
-- 9 : (3 : (2 : (sieve [25])))
--                                                {apply sieve}
-- 9 : (3 : (2 : (25 (strikeMultiples 25 []))))
--                                                {apply strike Multiples}
-- 9 : (3 : (2 : (25 : [])))
-- [9, 3, 2, 25]

-- zip:
zipLists :: [a] -> [b] -> [(a, b)]
zipLists [] _ = []
zipLists _ [] = []
zipLists (x:xs) (y:ys) = (x,y) : zipLists xs ys

list15 = zipLists ['a', 'b', 'c'] [1, 2, 3, 4] -- [('a', 1), ('b', 2), ('c', 3)]
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

-- Eingebaut:
-- list16 = zip ['a', 'b', 'c'] [1, 2, 3, 4] -- [('a', 1), ('b', 2), ('c', 3)]

-- Mehrfache Rekursion:
-- quicksort:
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger = [b | b <- xs, b > x]

list16 = qsort [9, 18, 81, 27, 36, 90, 72, 45, 63, 54] -- [9, 18, 27, 36, 45, 54, 63, 72, 81, 90]
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
evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

-- Funktion, die alle Elemente an ungeraden Stellen aus einer Liste nimmt:
odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

list17 = evens "abcde" --"ace"
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

-- Funktionen höherer Ordnung:
composition :: (b -> c) -> (a -> b) -> (a -> c)
composition f g = \x -> f (g x)

-- Beispiel:
i :: Int -> Int
i a = 2 * a

j :: Int -> Int
j a = 3 * a

fkt1 = composition j i

x6 = fkt1 1 -- 6

-- Funktionen höherer Ordnung auf Listen:
-- map:
listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x:xs) = (f x) : (listMap f xs)

-- Bestimme die ersten 5 ungeraden Zahlen:
list18 = listMap (\x -> x * 2 + 1) [0, 1, 2, 3, 4] -- [1, 3, 5, 7, 9]
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

-- Eingebaut:
-- list18 = map (\x -> x * 2 + 1) [0, 1, 2, 3, 4] -- [1, 3, 5, 7, 9]

-- filter:
listFilter :: (a -> Bool) -> [a] -> [a]
listFilter p [] = []
listFilter p (x:xs) | p x = x : listFilter p xs
                    | otherwise = listFilter p xs

-- andere Möglichkeit für strikeMultiples:
strikeMultiples' :: Integer -> [Integer] -> [Integer]
strikeMultiples' n list = listFilter (\ el -> (el `rem` n) /= 0) list

list19 = strikeMultiples' 3 [1, 2, 3, 4, 5, 6, 7, 8, 9] -- [1, 2, 4, 5, 7, 8]
-- Zwischenschritte:
-- strikeMultiples' 3 [1, 2, 3, 4, 5, 6, 7, 8, 9]
--                                                  {apply strikeMultiples'}
-- 1 : (listFilter (\ el -> (el `rem` n) /= 0) [2, 3, 4, 5, 6, 7, 8, 9])
--                                                  {apply strikeMultiples'}
-- 1 : (2 : (listFilter (\ el -> (el `rem` n) /= 0) [3, 4, 5, 6, 7, 8, 9]))
--                                                  {apply strikeMultiples'}
-- 1 : (2 : (listFilter (\ el -> (el `rem` n) /= 0) [4, 5, 6, 7, 8, 9]))
--                                                  {apply strikeMultiples'}
-- 1 : (2 : (4 : (listFilter (\ el -> (el `rem` n) /= 0) [5, 6, 7, 8, 9])))
--                                                  {apply strikeMultiples'}
-- 1 : (2 : (4 : (5 : (listFilter (\ el -> (el `rem` n) /= 0) [6, 7, 8, 9]))))
--                                                  {apply strikeMultiples'}
-- 1 : (2 : (4 : (5 : (listFilter (\ el -> (el `rem` n) /= 0) [7, 8, 9]))))
--                                                  {apply strikeMultiples'}
-- 1 : (2 : (4 : (5 : (7 : (listFilter (\ el -> (el `rem` n) /= 0) [8, 9])))))
--                                                  {apply strikeMultiples'}
-- 1 : (2 : (4 : (5 : (7 : (8 : (listFilter (\ el -> (el `rem` n) /= 0) [9]))))))
--                                                  {apply strikeMultiples'}
-- 1 : (2 : (4 : (5 : (7 : (8 : (listFilter (\ el -> (el `rem` n) /= 0) []))))))
--                                                  {apply strikeMultiples'}
-- 1 : (2 : (4 : (5 : (7 : (8 : [])))))
-- [1, 2, 4, 5, 7, 8]

-- Eingebaut:
-- strikeMultiples' :: Integer -> [Integer] -> [Integer]
-- strikeMultiples' n list = filter (\ el -> (el `rem` n) /= 0) list

-- foldr:
listFoldr :: (a -> b -> b) -> b -> [a] -> b
listFoldr f v [] = v
listFoldr f v (x:xs) = f x (listFoldr f v xs)

-- andere Möglichkeit für sum:
sum' :: [Int] -> Int
sum' xs = listFoldr (+) 0 xs

x7 = sum' [1, 2, 3, 4, 5] -- 15
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

-- Eingebaut:
-- sum' :: [Int] -> Int
-- sum' xs = foldr (+) 0 xs

-- concatenate:
concatenate :: [a] -> [a] -> [a]
concatenate list1 list2 = listFoldr (:) list2 list1

list20 = concatenate [1, 2, 3] [4, 5] -- [1, 2, 3, 4, 5]
-- Zwischenschritte:
-- concatenate [1, 2, 3] [4, 5]
--                                                   {apply concatenate}
-- listFoldr (:) [4, 5] [1, 2, 3]
--                                                   {apply listFoldr}
-- (:) 1 (listFoldr (:) [4, 5] [2, 3])
--                                                   {apply listFoldr}
-- (:) 1 ((:) 2 (listFoldr (:) [4, 5] [3]))
--                                                   {apply listFoldr}
-- (:) 1 ((:) 2 ((:) 3 (listFoldr (:) [4, 5] [])))
--                                                   {apply listFoldr}
-- (:) 1 ((:) 2 ((:) 3 [4, 5]))
-- [1, 2, 3, 4, 5]

-- Eingebaut:
-- list20 = [1, 2, 3] ++ [4, 5]

-- compose:
compose :: [a -> a] -> (a -> a)
compose = listFoldr (.) id

f :: Int -> Int
f a = 2 * a

g :: Int -> Int
g a = 3 * a

h :: Int -> Int
h a = 4 * a

fkt2 :: Int -> Int
fkt2 = compose [f, g, h] -- f . g . h
x8 = fkt2 1 -- 24
-- Zwischenschritte:
-- compose [f, g, h]
--                                                    {apply compose}
-- listFoldr (.) id [f, g, h]
--                                                    {apply listFoldr}
-- (.) f (listFoldr (.) id [g, h])
--                                                    {apply listFoldr}
-- (.) f ((.) g (listFoldr (.) id [h]))
--                                                    {apply listFoldr}
-- (.) f ((.) g (listFoldr (.) id [h]))
--                                                    {apply listFoldr}
-- (.) f ((.) g ((.) h (listFoldr (.) id [])))
--                                                    {apply listFoldr}
-- (.) f ((.) g ((.) h id))
-- f . g . h

-- foldl:
listFoldl :: (a -> b -> b) -> b -> [a] -> b
listFoldl f v [] = v
listFoldl f v (x:xs) = listFoldl f (f x v) xs

-- andere Möglichkeit für sum:
sum'' :: [Int] -> Int
sum'' xs = listFoldl (+) 0 xs

x9 = sum'' [1, 2, 3, 4, 5] -- 15
-- Zwischenschritte:
-- sum'' [1, 2, 3, 4, 5]
--                                                    {apply sum''}
-- listFoldl (+) 0 [1, 2, 3, 4, 5]
--                                                    {apply listFoldl}
-- listFoldl (+) ((+) 1 0) [2, 3, 4, 5]
--                                                    {apply listFoldl}
-- listFoldl (+) ((+) 2 ((+) 1 0)) [3, 4, 5]
--                                                    {apply listFoldl}
-- listFoldl (+) ((+) 3 ((+) 2 ((+) 1 0))) [4, 5]
--                                                    {apply listFoldl}
-- listFoldl (+) ((+) 4 ((+) 3 ((+) 2 ((+) 1 0)))) [5]
--                                                    {apply listFoldl}
-- listFoldl (+) ((+) 5 ((+) 4 ((+) 3 ((+) 2 ((+) 1 0))))) []
--                                                    {apply listFoldl}
-- ((+) ((+) ((+) ((+) ((+) 0 1) 2) 3) 4) 5)
-- 15

-- Eingebaut:
-- sum'' :: [Int] -> Int
-- sum'' xs = foldl (+) 0 xs

-- Halbgruppen, Monoide, Funktoren, Foldables
-- Map:
data Map key value = Map [(key, value)]
 deriving Show

-- Beispiel:
map1 :: Map String String
map1 = Map [("Mike", "Sperber"), ("Angela", "Merkel")]

unMap (Map list) = list

list21 = unMap map1 -- [("Mike","Sperber"),("Angela","Merkel")]

-- mapMap:
mapMap :: (a -> b) -> (Map key) a -> (Map key) b
mapMap f (Map []) = Map []
mapMap f (Map ((key, a):rest)) =
--    let Map frest = mapMap f (Map rest)
--    in Map ((key, f a):frest)
    Map ((key, f a):unMap (mapMap f (Map rest)))

-- Beispiel:
map2 = mapMap reverse map1 -- Map [("Mike","rebrepS"),("Angela","lekreM")]

-- key-value-Paar in eine Map einfügen:
mapPut :: key -> value -> Map key value -> Map key value
mapPut key value (Map list) = Map ((key, value) : list)

-- Beispiel:
map3 = mapPut "Erika" "Bor" map1 -- Map [("Erika","Bor"),("Mike","Sperber"),("Angela","Merkel")]

-- Optional, manchmal da, manchmal nicht:
data Optional a =
    NotThere
  | There a
 deriving Show

-- Beispiel:
opt1 :: Optional String
opt1 = There "Schokoladensahnetorte"

opt2 :: Optional String
opt2 = NotThere

-- Eingebaut:
-- data Maybe a = Nothing | Just a

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f NotThere  = NotThere
optionalMap f (There a) = There (f a)

-- Beispiel:
opt3 = optionalMap (++ " mit Sahne") opt1 -- There "Schokoladensahnetorte mit Sahne"

opt4 = optionalMap (++ " mit Sahne") opt2 -- NotThere

-- Eintrag in Map suchen
mapGet :: Eq key => -- wenn key vergleichbar ist ...
            key -> Map key value -> Optional value
mapGet key (Map []) = NotThere
mapGet key' (Map ((key, value):rest)) =
    if key' == key
    then There value
    else mapGet key' (Map rest)

-- Beispiel:
opt5 = mapGet "Erika" map1 -- NotThere

opt6 = mapGet "Erika" map3 -- "Bor"

-- Halbgruppe / Semigroup: Menge + Kombinator + Assoziativgesetz
class Semigroup (a :: *) where
    combine :: a -> a -> a
    -- (a `combine` b) `combine` c = a `combine (b `combine` c)

-- Die Menge Int erfüllt mit dem Kombinator (+) das Assoziativgesetz und ist eine Halbgruppe.
-- (+) :: Integer -> Integer -> Integer
-- (a + b) + c = a + (b + c) -- Assoziativgesetz
instance Semigroup Integer where
    combine :: Integer -> Integer -> Integer
    combine = (+)

-- andere Möglichkeit Int zu einer Halbgruppe zu machen:

newtype MultInt = Mult Int
  deriving (Show, Eq, Ord)

-- Die Menge Int erfüllt mit dem Kombinator (*) das Assoziativgesetz und ist eine Halbgruppe.
-- (*) :: Integer -> Integer -> Integer
-- (a * b) * c = a * (b * c)
instance Semigroup MultInt where
    combine (Mult a) (Mult b) = Mult (a * b)

-- Die Menge [a] erfüllt mit dem Kombinator (++) das Assoziativgesetz und ist eine Halbgruppe.
-- (++) :: [a] -> [a] -> [a]
-- (l1 ++ l2) ++ l3 = l1 ++ (l2 ++ l3)
instance Semigroup [a] where
    combine l1 l2 = l1 ++ l2

-- Die Menge (a -> a) erfüllt mit dem Kombinator (.) das Assoziativgesetz und ist eine Halbgruppe.
-- (.) :: (a -> a) -> (a -> a) -> (a -> a)
-- (f . g) . h = f . ( g . h)
instance Semigroup ((->) a a) where
    combine f g = f . g

-- True  && True = True
-- True  && False = False
-- False && True = False
-- False && False = False

-- Die Menge Bool erfüllt mit dem Kombinator (&&) das Assoziativgesetz und ist eine Halbgruppe.
-- (&&) :: Bool -> Bool -> Bool
-- (b1 && b2) && b3 = b1 && (b2 && b3)
instance Semigroup Bool where
    combine b1 b2 = b1 && b2

-- andere Möglichkeit, Bool zu einer Halbgruppe zu machen:

-- True  || True = True
-- True  || False = True
-- False || True = True
-- False || False = False

newtype DisjunctiveBool = Disjunctive Bool
  deriving (Show, Eq, Ord)

-- Die Menge DisjunctiveBool erfüllt mit dem Kombinator (||) das Assoziativgesetz und ist eine Halbgruppe.
-- (||) :: Bool -> Bool -> Bool
-- (b1 || b2) || b3 = b1 || (b2 || b3)
instance Semigroup DisjunctiveBool where
    combine (Disjunctive b1) (Disjunctive b2) = Disjunctive (b1 || b2)

-- Ist a eine Halbgruppe, dann ist auch Optional a eine Halbgruppe
instance Semigroup a => Semigroup (Optional a) where
    combine NotThere (There a1) = There a1  
    combine (There a1) NotThere = There a1  
    combine (There a1) (There a2) = There (combine a1 a2)  

-- Tupeln von Halbgruppen sind wieder Halbgruppen.
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
    combine (a1, b1) (a2, b2) =
        (combine a1  a2, combine b1 b2)

-- neutrales Element
-- n + 0 = 0 + n = n -- bzgl. (+)
-- n * 1 = 1 * n = n -- bzgl. (*)
-- [] ++ l = l ++ [] -- bzgl. (++)

-- Monoid: Halbgruppe + neutrales Element
class Semigroup a => Monoid a where
    neutral :: a

-- Die Halbgruppe Int bzgl. (+) ist mit 0 ein Monoid.
instance Monoid Integer where
    neutral = 0

-- Die Halbgruppe MultInt bzgl. (*) ist mit Mult 1 ein Monoid.
instance Monoid MultInt where
    neutral = Mult 1

-- Die Halbgruppe [a] ist mit [] ein Monoid.
instance Monoid [a] where
    neutral = []

-- Identität
identity :: a -> a
identity a = a

-- Die Halbgruppe (a -> a) ist mit identity ein Monoid.
instance Monoid ((->) a a) where
    neutral = identity

-- Die Halbgruppe Bool bzgl. && ist mit True ein Monoid
instance Monoid Bool where
    neutral = True

-- Die Halbgruppe Bool bzgl. || ist mit False ein Monoid
instance Monoid DisjunctiveBool where
    neutral = Disjunctive False

-- Ist a eine Halbgruppe (?), dann ist die Halbgruppe Optional a mit NotThere ein Monoid
instance Semigroup a => Monoid (Optional a) where
    neutral = NotThere

-- Tupeln von Monoiden sind wieder Monoide
--instance (Monoid a, Monoid b) => Monoid (a, b) where
--    neutral = ??

-- Funktor
class Functor (constructor :: * -> *) where
    mmap :: (a -> b) -> constructor a -> constructor b
    -- mmap identity = identity
    -- mmap (f . g) = (mmap f) . (mmap g)

-- f :: b -> c, g :: a -> b, f . g :: a -> c
-- mmap g :: constructor a -> constructor b, mmap f :: constructor b -> constructor c, (mmap f . mmap g) :: constructor a -> constructor c

instance Functor [] where
    mmap = map

instance Functor (Map key) where
    mmap = mapMap

instance Functor Optional where
    mmap = optionalMap

instance Functor ((->) r) where
    mmap = (.)
-- ((->) r) ist die Menge der Funktionen mit Domäne vom Typ r

-- 2 + 3, ((+) 2 3) , a -> b, ((->) a b)

-- mmap :: (a -> b) -> ((->) r a) -> ((->) r b) 

-- f :: a -> b, g :: r -> a

-- mmap f g = f . g

-- Beweis:
-- mmap id_a g = id_a . g = g = id_((->) a) g
-- h :: b -> c
-- mmap (h . f) g = (h . f) . g = h . (f . g) = mmap h (mmap f g) = (mmap h . mmap f) g

-- Beispiel:
fkt3 :: Int -> Int
fkt3 = mmap (*3) (+100)

x10 = fkt3 1 -- 303

-- Foldable
class Foldable (constructor :: * -> *) where
    myfoldr :: (a -> b -> b) -> b -> constructor a -> b

instance Foldable [] where
    myfoldr = listFoldr

optionalFoldr :: (a -> b -> b) -> b -> Optional a -> b
optionalFoldr f v NotThere = v
optionalFoldr f v (There a) = f a v

instance Foldable Optional where
    myfoldr = optionalFoldr