{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Map where

import Prelude hiding (Foldable, Functor, Semigroup, Monoid)

-- Map, Zusammenstellung von key-value-Paaren:
data Map key value = Map [(key, value)]
 deriving Show

-- Beispiel
map1 :: Map String String
map1 = Map [("Mike", "Sperber"), ("Angela", "Merkel")]

unMap :: Map key value -> [(key, value)]
unMap (Map list) = list

list21 :: [(String, String)]
list21 = unMap map1 -- [("Mike","Sperber"),("Angela","Merkel")]

-- Wende eine Funktion auf jeden value-Eintrag einer Map an:
mapMap :: (a -> b) -> (Map key) a -> (Map key) b
mapMap f (Map []) = Map []
mapMap f (Map ((key, a):rest)) =
--    let Map frest = mapMap f (Map rest)
--    in Map ((key, f a):frest)
    Map ((key, f a):unMap (mapMap f (Map rest)))

-- Beispiel
map2 :: Map String String
map2 = mapMap reverse map1 -- Map [("Mike","rebrepS"),("Angela","lekreM")]

-- key-value-Paar in eine Map einfügen:
mapPut :: key -> value -> Map key value -> Map key value
mapPut key value (Map list) = Map ((key, value) : list)

-- Beispiel
map3 :: Map String String
map3 = mapPut "Erika" "Bor" map1 -- Map [("Erika","Bor"),("Mike","Sperber"),("Angela","Merkel")]

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

-- Beispiel
opt5 :: Optional String
opt5 = mapGet "Erika" map1 -- NotThere

opt6 :: Optional String
opt6 = mapGet "Erika" map3 -- "Bor"

-- Eintrag in Map suchen:
mapGet :: Eq key => -- wenn key vergleichbar ist ...
            key -> Map key value -> Optional value
mapGet key (Map []) = NotThere
mapGet key' (Map ((key, value):rest)) =
    if key' == key
    then There value
    else mapGet key' (Map rest)

class Functor (constructor :: * -> *) where
    mmap :: (a -> b) -> constructor a -> constructor b
    -- mmap identity = identity
    -- mmap (f . g) = (mmap f) . (mmap g)

-- g :: a -> b, f :: b -> c, f . g :: a -> c
-- mmap g :: constructor a -> constructor b, mmap f :: constructor b -> constructor c, (mmap f . mmap g) :: constructor a -> constructor c

-- Der Typkonstruktor (Map key) ist mit der Abbildung mapMap ein Funktor
instance Functor (Map key) where
    mmap = mapMap