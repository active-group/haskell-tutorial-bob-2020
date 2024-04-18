{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Spickzettel where

import Prelude hiding (Foldable, Functor, Semigroup, Monoid)

-- Zwei Zahlen addieren
add :: Int -> Int -> Int
add x y = x + y

-- curried function - Beispiele
func1 :: Int -> Int
func1 = add 1

result1 :: Int
result1 = func1 2 -- 3

func2 :: Int -> Int
func2 = add 2

result2 :: Int
result2 = func2 2 -- 4

add' :: Int -> (Int -> Int)
add' x = add x
-- add' x y = add x y = x + y

-- Listen ([] ist ein parametrisierter Typkonstruktor):

-- Beispiele
list1 :: [Int]
list1 = [5] -- 1-elementige Liste: 5

list2 :: [Int]
list2 = [5, 7] -- 2-elementige Liste: 5, 7

list3 :: [Int]
list3 = 12:list2 -- 3-elementige Liste: 12 5 7, (:) fügt ein Element vorne an die Liste an

list4 :: [Bool]
list4 = [False, True, False] -- 3-elementige Liste: False, True, False

list5 :: [Char]
list5 = ['a', 'b', 'c', 'd'] -- 4-elementige Liste: a, b, c, d

list6 :: [String]
list6 = ["One", "Two", "Three"] -- 3-elementige Liste: One, Two, Three

list7 :: [[Char]]
list7 = [['a', 'b'], ['c', 'd', 'e']] -- 2-elementige Liste: ['a', 'b'], ['c', 'd', 'e']

-- Funktionen auf Listen:
x1 :: Integer
x1 = sum [1, 2, 3, 4, 5] -- 15, Addiere Einträge in einer Liste auf

x2 :: Integer
x2 =  head [1, 2, 3, 4, 5] -- 1, erster Eintrag aus der Liste

list8 :: [Integer]
list8 = tail [1, 2, 3, 4 ,5] -- [2, 3, 4 ,5], Liste ohne den ersten Eintrag

x3 :: Integer
x3 = [1, 2, 3, 4, 5] !! 2 -- 3, dritter Eintrag aus der Liste

list9 :: [Integer]
list9 = take 3 [1, 2, 3, 4, 5] -- [1, 2, 3], die ersten drei Einträge aus der Liste

list10 :: [Integer]
list10 = drop 3 [1, 2, 3, 4, 5] -- [4, 5], Liste ohne die ersten drei Einträge

x4 :: Int
x4 = length [1, 2, 3, 4, 5] -- 5, Anzahl der Einträge in einer Liste

x5 :: Integer
x5 = product [1, 2, 3, 4, 5] -- 120, multipliziere Einträge in einer Liste auf

list11 :: [Integer]
list11 = [1, 2, 3] ++ [4, 5] -- [1, 2, 3, 4, 5], verbinde zwei Listen

list12 :: [Integer]
list12 = reverse [1, 2, 3, 4, 5] -- [5, 4, 3, 2, 1], drehe die Reihenfolge der Einträge in einer Liste um

-- Spiele mit Funktionen auf Listen:
-- Teile Liste nach dem n-ten Element:
splitListAt :: Int -> [a] -> ([a], [a])
splitListAt n xs = (take n xs, drop n xs)

tuple1 :: ([Integer], [Integer])
tuple1 = splitListAt 3 [1, 2, 3, 4, 5] -- ([1, 2, 3], [4, 5])

-- Eingebaut:
-- tuple1 = splitAt 3 [1, 2, 3, 4, 5] -- ([1, 2, 3], [4, 5])

-- Beispiele für Rekursion:

-- Führe Multiplikation auf wiederholte Addition zurück:
mult :: Int -> Int -> Int
mult m 0 = 0
mult m n = m + (mult m (n-1))

-- Beispiel
x6 :: Int
x6 = mult 4 3 -- 12
-- Zwischenschritte:
-- mult 4 3
--                                    {apply mult}
-- 4 + (mult 4 2)
--                                    {apply mult}
-- 4 + (4 + (mult 4 1))
--                                    {apply mult}
-- 4 + (4 + (4 + (mult 4 0)))
--                                    {apply mult}
-- 4 + (4 + (4 + 0))
-- 12

-- Führe Potenzieren auf wiederholte Multiplikation zurück:
pow :: Int -> Int -> Int
pow m 0 = 1
pow m n = m * (pow m (n-1))

-- Beispiel
x7 :: Int
x7 = pow 4 3 -- 64
-- Zwischenschritte:
-- pow 4 3
--                                    {apply pow}
-- 4 * (pow 4 2)
--                                    {apply pow}
-- 4 * (4 * (pow 4 1))
--                                    {apply pow}
-- 4 * (4 * (4 * (pow 4 0)))
--                                    {apply pow}
-- 4 * (4 * (4 * 1))
-- 64

-- Berechne Fakultät:
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- Beispiel
x8 :: Int
x8 = fac 3 -- 6
-- Zwischenschritte:
-- fac 3
--                                     {apply fac}
-- 3 * (fac 2)
--                                     {apply fac}
-- 3 * (2 * (fac 1))
--                                     {apply fac}
-- 3 * (2 * (1 * (fac 0)))
--                                     {apply fac}
-- 3 * (2 * (1 * 1))
--6

-- Natürliche Zahlen ab n:
natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n + 1)

-- Natürliche Zahlen ab 0:
nats :: [Integer]
nats = natsFrom 0
-- Zwischenschritte:
-- natsfrom 0 = 0 : (1 : (2 : (3 : [...])))

-- Beispiele für Rekursion auf Listen:

-- Addiere Einträge in einer Liste auf
listSum :: [Int] -> Int
listSum [] = 0
listSum (first:rest) = first + (listSum rest)

x9 :: Int
x9 = listSum [1, 2, 3, 4, 5] -- 15
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

-- Vielfache von n aus einer Liste entfernen:
strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n [] = []
-- `rem` -- Funktion rem, nur Infix
strikeMultiples n (first:rest) =
    if (first `rem` n) == 0 -- first ist Vielfaches von n
    then strikeMultiples n rest
    else first : (strikeMultiples n rest)

list13 :: [Integer]
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

list14 :: [Integer]
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

-- Zwei Listen zippen:
zipLists :: [a] -> [b] -> [(a, b)]
zipLists [] _ = []
zipLists _ [] = []
zipLists (x:xs) (y:ys) = (x,y) : zipLists xs ys

list15 :: [(Char,Int)]
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

-- Eingebaut: zip

-- Mehrfache Rekursion:
-- quicksort:
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger = [b | b <- xs, b > x]

list16 :: [Int]
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

list17 :: String
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

-- Beispiele für Funktionen höherer Ordnung auf Listen:

-- Eine Funktion auf jeden Eintrag einer Liste anwenden: 
listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x:xs) = (f x) : (listMap f xs)

-- Bestimme die ersten 5 ungeraden Zahlen:
list18 :: [Int]
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

-- Eingebaut: map

-- Eine Liste filtern:
listFilter :: (a -> Bool) -> [a] -> [a]
listFilter p [] = []
listFilter p (x:xs) | p x = x : listFilter p xs
                    | otherwise = listFilter p xs

-- andere Möglichkeit für strikeMultiples:
strikeMultiples' :: Integer -> [Integer] -> [Integer]
strikeMultiples' n list = listFilter (\ el -> (el `rem` n) /= 0) list

list19 :: [Integer]
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

-- Eingebaut: filter

-- Die Einträge einer Liste miteinander verknüpfen, am Ende der Liste angefangen:
listFoldr :: (a -> b -> b) -> b -> [a] -> b
listFoldr f v [] = v
listFoldr f v (x:xs) = f x (listFoldr f v xs)

-- andere Möglichkeit für sum:
sum' :: [Int] -> Int
sum' xs = listFoldr (+) 0 xs

x11 :: Int
x11 = sum' [1, 2, 3, 4, 5] -- 15
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

-- Eingebaut: foldr

-- Zwei Listen zusammenkleben:
concatenate :: [a] -> [a] -> [a]
concatenate list1 list2 = listFoldr (:) list2 list1

list20 :: [Int]
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

-- Eingebaut: (++)

-- Eine Liste von Funktionen von a nach a verketten:
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

x12 :: Int
x12 = fkt2 1 -- 24
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

-- Die Einträge einer Liste miteinander verknüfen, am Anfang der Liste angefangen:
listFoldl :: (a -> b -> b) -> b -> [a] -> b
listFoldl f v [] = v
listFoldl f v (x:xs) = listFoldl f (f x v) xs

-- andere Möglichkeit für sum:
sum'' :: [Int] -> Int
sum'' xs = listFoldl (+) 0 xs

x13 :: Int
x13 = sum'' [1, 2, 3, 4, 5] -- 15
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

-- Eingebaut: foldl