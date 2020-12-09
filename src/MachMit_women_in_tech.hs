{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module MachMit_women_in_tech where

import Prelude hiding (Foldable, Functor, Semigroup, Monoid)

-- Basistypen in Haskell
-- Bool: False, True
-- Char, z.B. 'a', 'A', '3', '_', '\n', '\t'
-- String, z.B. "abc", "1+2+3"
-- Int, z.B. -100, 99, 0
-- Integer
-- Float, z.B. -12.34, 1.0, 3.1415927
-- Double
-- Tupel (T1, ..., Tn), () leerer Tupel (Sequenz von Komponenten, die je unterschiedliche Typen annehmen können) (1, 3.14, "Erika")
-- Listen (später?)
-- State: Solid, Liquid, Gas


-- Variablen
z :: Integer
z = 3


-- Funktionen
-- Eine Zahl verdoppeln
double :: Int -> Int
double x = x * 2


-- Zwei Zahlen addieren
add :: Int -> Int -> Int
add x y = x + y



-- curried functions

-- Aggregatzustand von chemischen Elementen
-- Ein Aggregatzustand ist eins der folgenden:
-- gasförmig
-- flüssig
-- fest
data State = Gas | Liquid | Solid
  deriving Show

-- data GanzeZahlen = .. -1 | 0 | 1 ...
-- data Bool = True | False







-- Agreggatzustand von Wasser
computeState :: Float -> State
{-computeState t =
    if t < 0
    then Solid
     else
        if (t > = 0) && (t <= 100)
        then Liquid
        else Gas-}
computeState t
  | t < 0 = Solid
  | (t >= 0) && (t <= 100) = Liquid
  | otherwise = Gas

 --Eq: Int, Char, String, Bool, ...
 --Ord: Int, Char, String, Bool, ...
 --Show: Int, Char, String, Bool, ... 

-- Typische Temperatur von z.B. Wasser
typicalTemp :: State -> Float
typicalTemp Gas = 150.0
typicalTemp Liquid = 20.0
typicalTemp Solid = -20.0



-- Typische Temperatur




-- Zusammengesetzte Daten
-- Ein Gürteltier hat die folgenden Eigenschaften:
-- Lebendigkeit
-- Gewicht
data Dillo = Dillo {dilloLiveness :: Liveness,
                    dilloWeight :: Int}
   deriving (Show, Eq)

--dilloWeight :: Dillo -> Int

dillo1 :: Dillo
dillo1 = Dillo Alive 10
dillo2 :: Dillo
dillo2 = Dillo Dead 12












-- Lebendigkeit kann folgende Werte annehmen
-- tot
-- lebendig
data Liveness = Dead | Alive
  deriving (Show, Eq)

-- Gürteltiere füttern
feedDillo :: Int -> Dillo -> Dillo
feedDillo amount (Dillo Alive weight) = Dillo Alive (weight + amount)
feedDillo amount (Dillo Dead weight) = Dillo Dead weight

-- Gürteltier überfahren




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
parrot3 :: Parrot
parrot3 = feedParrot 1 parrot1 -- Parrot "Der Schatz ist im Silbersee!" 6 -}

{-
-- Algebraischer Datentyp: Gemischte Daten von zusammengesetzten Daten
-- Ein Tier ist entweder ein Dillo oder ein Papagei:
data Animal = Dillo {dilloLiveness :: Liveness,
                    dilloWeight :: Int}
             |Parrot {parrotSentence :: String,
                      parrotWeight :: Int}
    deriving (Show, Eq) 



-- Beispiele
dillo1 :: Animal
dillo1 = Dillo Alive 10
dillo2 :: Animal
dillo2 = Dillo Dead 12
parrot1 :: Animal
parrot1 = Parrot "Der Schatz ist am Silbersee" 5

-- Tiere füttern
feedAnimal :: Int -> Animal -> Animal
feedAnimal amount (Dillo Alive weight) = Dillo Alive (weight + amount)
feedAnimal amount (Dillo Dead weight) = Dillo Dead weight
feedAnimal amount (Parrot _ weight) = Parrot _ (weight + amount)


-- Tiere füttern
-}


--Beispiele



-- Rekursion:
-- Führe Multiplikation zweier Zahlen auf wiederholte Addition zurück:
mult :: Int -> Int -> Int
mult m 0 = 0
mult m n = m + (mult m (n-1))

-- Beispiel
x6 :: Int
x6 = mult 4 3 -- 12
-- Zwischenschritte:
-- mult 4 3
-- = 4 + mult 4 2
-- = 4 + 4 + mult 4 1
-- = 4 + 4 + 4 + mult 4 0
-- = 4 + 4 + 4 + 0
-- = 12

-- Fakultät einer Zahl
