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
-- Tupel (T1, ..., Tn), () leerer Tupel

-- Variablen
z = 3

-- Funktionen
-- Zahl verdoppeln
double :: Int -> Int
double x = x * 2

-- Zwei Zahlen addieren
add :: Int -> Int -> Int
add x y = x + y

-- curried function - Beispiele


-- Ein Aggregatzustand ist eins der folgenden:
-- - gasförmig
-- - flüssig
-- - fest
data State = Gas | Liquid | Solid

-- Agreggatzustand von Wasser
-- guards
computeState :: Float -> State
computeState t =
  if t < 0
  then Solid
  else
    if (t >= 0) && (t <= 100)
    then Liquid
      else Gas

{- computeState t
    | t < 0 = Solid
    | (t >= 0) && (t <= 100) = Liquid
    | otherwise = Gas -}

-- Typische Temperatur
-- pattern matching
typicalTemp :: State -> Float
typicalTemp state =
    case state of
        Gas -> 150
        Liquid -> 20
        Solid -> -20

{- typicalTemp Gas = 150
typicalTemp Liquid = 20
typicalTemp Solid = -20 -}

-- Eine Form ist eins der folgenden:
-- - Kreis
-- - Rechteck
data Shape = Circle Float | Rect Float Float

-- Wir erstellen ein Quadrat
square :: Float -> Shape
square n = Rect n n

-- Wir berechnen die Fläche
-- pattern matching
area :: Shape -> Float
area shape =
    case shape of
        Circle r -> pi * r^2
        Rect x y -> x * y

{- area (Circle r) = pi * r^2
area (Rect x y) = x * y -}

-- Wir berechnen den Umfang
-- pattern matching
circumference :: Shape -> Float
circumference shape =
    case shape of
        Circle r -> 2 * pi * r
        Rect x y -> 2 * x + 2 * y

{- circumference (Circle r) = 2 * pi * r
circumference (Rect x y) = 2 * x + 2 * y -}

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
data Dillo = Dillo Liveness Int
    deriving (Show, Eq)

-- Beispiele
dillo1 :: Dillo
dillo1 = Dillo Alive 10 -- lebendiges Gürteltier, 10kg
dillo2 :: Dillo
dillo2 = Dillo Dead 12 -- totes Gürteltier, 12kg

-- Lebt der Dillo?
-- wildcard pattern
liveness :: Dillo -> Liveness
liveness (Dillo Alive _) = Alive
liveness (Dillo Dead _) = Dead

-- Beispiele
res1 :: Liveness
res1 = liveness dillo1 -- Alive
res2 :: Liveness
res2 = liveness dillo2 -- Dead

-- Gewicht des Dillos?
-- wildcard pattern
weight :: Dillo -> Int
weight (Dillo _ w) = w

-- Beispiel
res3 :: Int
res3 = weight dillo1 -- 10
 
{- -- Besser: Record Syntax
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

{- -- Algebraischer Datentyp: Gemischte Daten von zusammengesetzten Daten
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
parrot2 = feedAnimal 1 parrot1 -- -- Parrot "Der Schatz ist im Silbersee!" 6 -}

-- Listen ([] ist ein parametrisierter Typkonstruktor):

-- Beispiele


-- Funktionen auf Listen:


-- Listen sind rekursive Datentypen:
-- Eine Liste ist entweder die leere Liste [] oder von der Form x:xs
-- (wobei wenn x vom Typ a ist, ist xs vom Typ [a])

-- Beispiel
-- [1, 2, 3]
--                            {list notation}
-- 1 : [2, 3]
--                            {list notation}
-- 1 : (2 : [3])
--                            {list notation}
-- 1 : (2 : (3 : []))

-- Rekursion:


-- Rekursion auf Listen:


-- Funktionen höherer Ordnung:

-- Eine Funktion, die gleich zwei Funktionen als Argumente nimmt:
-- Lambda-Ausdruck
composition :: (b -> c) -> (a -> b) -> (a -> c)
composition f g = \x -> f (g x)

-- Beispiel:
i :: Int -> Int
i a = 2 * a

j :: Int -> Int
j a = 3 * a

fkt1 :: Int -> Int
fkt1 = composition j i

x10 :: Int
x10= fkt1 1 -- 6

-- Funktionen höherer Ordnung auf Listen:

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

-- Halbgruppen, Monoide, Funktoren, Foldables

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

-- Eintrag in Map suchen:
mapGet :: Eq key => -- wenn key vergleichbar ist ...
            key -> Map key value -> Optional value
mapGet key (Map []) = NotThere
mapGet key' (Map ((key, value):rest)) =
    if key' == key
    then There value
    else mapGet key' (Map rest)

-- Beispiel
opt5 :: Optional String
opt5 = mapGet "Erika" map1 -- NotThere

opt6 :: Optional String
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
    combine NotThere NotThere = NotThere 
    combine NotThere (There a1) = There a1  
    combine (There a1) NotThere = There a1  
    combine (There a1) (There a2) = There (combine a1 a2)  

-- Tupel von Halbgruppen sind wieder Halbgruppen.
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

-- Funktor
class Functor (constructor :: * -> *) where
    mmap :: (a -> b) -> constructor a -> constructor b
    -- mmap identity = identity
    -- mmap (f . g) = (mmap f) . (mmap g)

-- g :: a -> b, f :: b -> c, f . g :: a -> c
-- mmap g :: constructor a -> constructor b, mmap f :: constructor b -> constructor c, (mmap f . mmap g) :: constructor a -> constructor c

-- Der Typkonstruktor [] ist mit der Abbildung listMap ein Funktor
instance Functor [] where
    mmap = listMap

-- Der Typkonstruktor (Map key) ist mit der Abbildung mapMap ein Funktor
instance Functor (Map key) where
    mmap = mapMap

-- Der Typkonstruktor Optional ist mit der Abbildung optionalMap ein Funktor
instance Functor Optional where
    mmap = optionalMap

-- Der Typkonstruktor ((->) r) ist mit der Abbildung (.) ein Funktor
instance Functor ((->) r) where
    mmap = (.)

-- Schreibweise:
-- ((->) r) ist die Menge der Funktionen mit Domäne vom Typ r

-- 2 + 3, ((+) 2 3) , a -> b, ((->) a b)

-- Signatur von mmap:
-- mmap :: (a -> b) -> ((->) r a) -> ((->) r b) 
-- Für f :: a -> b und  g :: r -> a soll eine Funktion r -> b rauskommen
-- mmap f g = f . g 

-- Beweis:
-- mmap id_a g = id_a . g = g = id_((->) r a) g
-- Sei h :: b -> c
-- mmap (h . f) g = (h . f) . g = h . (f . g) = mmap h (mmap f g) = (mmap h . mmap f) g

-- Beispiel:
fkt3 :: Int -> Int
fkt3 = mmap (*3) (+100)

x14 :: Int
x14 = fkt3 1 -- 303

-- Foldable
class Foldable (constructor :: * -> *) where
    myfoldr :: (a -> b -> b) -> b -> constructor a -> b

-- Der Typkonstruktor [] ist mit der Abbildung listFoldr ein Foldable
instance Foldable [] where
    myfoldr = listFoldr

optionalFoldr :: (a -> b -> b) -> b -> Optional a -> b
optionalFoldr f v NotThere = v
optionalFoldr f v (There a) = f a v

-- Der Typkonstruktor [] ist mit der Abbildung optionalFoldr ein Foldable
instance Foldable Optional where
    myfoldr = optionalFoldr