{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Spickzettel_women_in_tech where

import Prelude hiding (Foldable, Functor, Semigroup, Monoid)

-- Basistypen in Haskell
-- Bool: False, True
-- Char, z.B. 'a', 'A', '3', '_', '\n', '\t'
-- String, z.B. "abc", "1+2+3"
-- Int, z.B. -100, 99, 0
-- Integer
-- Float, z.B. -12.34, 1.0, 3.1415927
-- Double
-- Tupel (T1, ..., Tn), () leerer Tupel (Sequenz von Komponenten, die je unterschiedliche Typen annehmen können)
-- Listen (später)

-- Variablen
z :: Integer
z = 3

-- Funktionen
-- Zahl verdoppeln
double :: Int -> Int
double x = x * 2
-- \x -> x * 2

-- Zwei Zahlen addieren
add :: Int -> Int -> Int
add x y = x + y

-- curried function - Beispiele
-- Typinferenz: Haskell schlägt hier für func1 automatisch Int -> Int vor.
-- Wenn add :: Int -> Int -> Int ist und bereits ein 1 :: Int eingesetzt ist (= func1),
-- bleibt für func1 keine andere Möglichkeit.
-- Allgemein gilt: Ist f :: a -> b und x :: a, dann ist f x :: b.

func1 :: Int -> Int
func1 = add 1
-- func1 y = add 1 y = 1 + y

-- Ein Aggregatzustand ist eins der folgenden:
-- - gasförmig
-- - flüssig
-- - fest
data State = Gas | Liquid | Solid
    deriving Show

-- data GanzZahlen = ... -1 | 0 | 1 ...
-- data Bool = True | False

-- Agreggatzustand von Wasser
-- guards
computeState :: Float -> State
{- computeState t =
  if t < 0
  then Solid
  else
    if (t >= 0) && (t <= 100)
    then Liquid
      else Gas -}
computeState t
    | t < 0 = Solid
    | (t >= 0) && (t <= 100) = Liquid
    | otherwise = Gas

-- Typische Temperatur
-- pattern matching
typicalTemp :: State -> Float
{-typicalTemp state =
    case state of
        Gas -> 150
        Liquid -> 20
        Solid -> -20 -}
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
res4 :: Liveness
res4 = dilloLiveness dillo1 -- Alive
res5 :: Liveness
res5 = dilloLiveness dillo2 -- Dead
res6 :: Int
res6 = dilloWeight dillo1 -- 10

-- Gürteltier füttern
feedDillo :: Int -> Dillo -> Dillo
feedDillo amount (Dillo Alive weight) = Dillo Alive (weight + amount)
feedDillo amount (Dillo Dead weight) = Dillo Dead weight

-- Beispiele
dillo3 :: Dillo
dillo3 = feedDillo 1 dillo1 -- Dillo Alive 11
dillo4 :: Dillo
dillo4 = feedDillo 1 dillo2 -- Dillo Dead 12

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
runOverDillo (Dillo _ weight) = Dillo Dead weight

-- Beispiele
dillo5 :: Dillo
dillo5 = runOverDillo dillo3 -- Dillo Dead 11
dillo6 :: Dillo
dillo6 = runOverDillo dillo2 -- Dillo Dead 12

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
feedAnimal amount (Dillo Alive weight) =
    Dillo Alive (weight + amount)
feedAnimal amount (Dillo Dead weight) =
    Dillo Dead weight
feedAnimal amount (Parrot sentence weight) =
    Parrot sentence (weight + amount)

-- Beispiele
dillo3 :: Animal
dillo3 = feedAnimal 1 dillo1 -- Dillo Alive 11
dillo4 :: Animal
dillo4 = feedAnimal 2 dillo2 -- Dillo Dead 12
parrot2 :: Animal
parrot2 = feedAnimal 1 parrot1 -- -- Parrot "Der Schatz ist im Silbersee!" 6
-}

-- Rekursion:
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