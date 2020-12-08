{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module MachMit where

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
-- Listen (später?)

-- Variablen


-- Funktionen
-- Eine Zahl verdoppeln


-- Zwei Zahlen addieren


-- curried functions

-- Aggregatzustand von chemischen Elementen




-- Agreggatzustand von Wasser


-- Typische Temperatur




-- Zusammengesetzte Daten
-- Ein Gürteltier hat die folgenden Eigenschaften:




-- Lebendigkeit kann folgende Werte annehmen




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


-- Algebraischer Datentyp: Gemischte Daten von zusammengesetzten Daten
-- Ein Tier ist entweder ein Dillo oder ein Papagei:


-- Beispiele


-- Tiere füttern


-- Beispiele



-- Rekursion:
-- Führe Multiplikation zweier Zahlen auf wiederholte Addition zurück:
mult :: Int -> Int -> Int
mult m 0 = 0
mult m n = m + (mult m (n-1))

-- Beispiel
x6 :: Int
x6 = mult 4 3 -- 12
-- Zwischenschritte:

-- Fakultät einer Zahl
