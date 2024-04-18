{-
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
-}

module IntroMachMit where

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

-- Funktionen
-- Zahl verdoppeln

-- Zwei Zahlen addieren

-- curried functions, Typinferenz

-- Ein Aggregatzustand ist eins der folgenden:
-- - gasförmig
-- - flüssig
-- - fest

-- data GanzeZahlen = ... -1 | 0 | 1 ...
-- data Bool = True | False

-- Agreggatzustand von Wasser, guards

-- Typische Temperatur, pattern matching

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

-- Beispiele

-- Gürteltier füttern, wildcard pattern

-- Beispiele

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

-- Algebraischer Datentyp: Gemischte Daten von zusammengesetzten Daten
-- Ein Tier ist entweder ein Dillo oder ein Papagei:

-- Beispiele

--Tiere füttern

-- Listen, parametrisierter Typkonstruktor,, rekursiver Datentyp, gemischte Daten

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

-- Eingebaut: sum

-- Zwei Listen zippen:
-- also sowas: zipLists ['a', 'b', 'c'] [1, 2, 3, 4] = [('a', 1), ('b', 2), ('c', 3)]  

-- Eingebaut: zip

-- Mehrfache Rekursion:
-- quicksort:
-- also sowas: qsort [9,18,81,90,72] = [9, 18, 72, 81, 90]

-- Wechselnde Rekursion:
-- Funktion, die alle Elemente an geraden Stellen aus einer Liste nimmt:
-- also sowas: evens "abcde" = ...

-- Funktion, die alle Elemente an ungeraden Stellen aus einer Liste nimmt:
-- also sowas: odds "bcde" = ...

-- Funktionen höherer Ordnung auf Listen:

-- Eine Funktion auf jeden Eintrag einer Liste anwenden: 
-- Beispiel: Bestimme die ersten 5 ungeraden Zahlen
-- also sowas: listMap ... = ...

-- Eingebaut: map

-- Die Einträge einer Liste miteinander verknüpfen, am Ende der Liste angefangen:
-- Beispiel: andere Möglichkeit für sum:
-- also sowas: sum' [1,2,3,4,5] = 15

-- Eingebaut: foldr

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





