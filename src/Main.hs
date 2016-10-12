module Main where

import Data.List
import Data.Char

-- Hello world --

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn "ehh"

-- Fib versions --

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

--

fibWithGuards :: Integer -> Integer
fibWithGuards x
    | x < 2      = 1
    | otherwise  = fibWithGuards (x - 1) + fibWithGuards (x - 2)


--

fibNextPair :: (Integer, Integer) -> (Integer, Integer)
fibNextPair (x, y) = (y, x + y)

fibNthPair :: Integer -> (Integer, Integer)
fibNthPair x
    | x == 1 = (1, 1)
    | otherwise = fibNextPair (fibNthPair (x - 1))

fib2 :: Integer -> Integer
fib2 = fst . fibNthPair


-- Some version off allEven --

allEven :: [Integer] -> [Integer]
allEven [] = []
allEven (h:tail) = if odd h then allEven tail else h:allEven tail

--

allEven2 :: [Integer] -> [Integer]
allEven2 = filter even

--

allEven3 :: [Integer] -> [Integer]
allEven3 xs = [x | x <- xs, even x]

-- Comprehension fun --

states = ["on", "off"]
trumpetCombinations = [(a, b, c) | a <- states, b <- states, c <- states]
possibleCombinations = length trumpetCombinations


persons = ["mikael", "martin", "susannsusanna", "Johanna", "erik"]
possibleDuos = [(a, b) | a <- persons, b <- persons, a /= b, a < b]

multiplicationTable = [(a, b, a * b) | a <- [0..10], b <- [0..10]]

-- Reimplementing haskell funktions --

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse'(xs) ++ [x]

sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' (x:xs) = smaller ++ [x] ++ bigger
    where smaller = sort' (filter (<x) xs)
          bigger = sort' (filter (>=x) xs)

sortBy' :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
sortBy' predicate [] = []
sortBy' predicate (x:xs) = smaller ++ [x] ++ bigger
    where smaller = sortBy' predicate (filter (\y -> predicate x y == GT) xs)
          bigger = sortBy' predicate (filter (\y -> let res = predicate x y in res == LT || res == EQ) xs)



-- Trying to do some side effects --

printTripple :: ([Char], [Char], [Char]) -> IO ()
printTripple (x, y, z) = do
    putStrLn (x ++ " | " ++ y ++ " | " ++ z)


printTrumpetCombinations :: [([Char], [Char], [Char])] -> IO ()
printTrumpetCombinations [] = do putStrLn "done"
printTrumpetCombinations (x:xs) = do
    printTripple x
    printTrumpetCombinations xs

-- Coloring us states --

colors = ["red", "green", "blue"]
usStates = ["alabama", "missisippi", "georgia", "tennessee", "florida"]

allStateCombinations = [
        zip usStates [alabama, missisippi, georgia, tennessee, florida]
        | alabama <- colors, missisippi <- colors, georgia <- colors, tennessee <- colors, florida <- colors
        , alabama /= missisippi
        , alabama /= missisippi
        , alabama /= missisippi
        , alabama /= tennessee
        , alabama /= georgia
        , alabama /= florida
        , missisippi /= tennessee
        , tennessee /= georgia
        , georgia /= florida
    ]

printCombinations = printRows allStateCombinations
    where printRows [] = putStrLn ""
          printRows (x:xs) = do
            mapM printPair x
            putStrLn ""
            putStrLn "---"
            putStrLn ""
            printRows xs

printPair (x, y) = putStrLn (x ++ " | " ++ y)

-- Number format --

chunksOf :: Int -> [a] -> [[a]]
chunksOf int [] = []
chunksOf int xs = [take int xs] ++ (chunksOf int (drop 3 xs))

addCommas :: [Char] -> [Char]
addCommas str = foldl (++) "" $ reverse . intersperse "," $ chunksOf 3 $ reverse str

formatNumber :: (Show a) => a -> String
formatNumber number = let (wholePoint, decimals) = break ((==) '.') (show number)
    in "$" ++ (addCommas wholePoint) ++ decimals

readFormated :: String -> Float
readFormated string = read trimmedDigit :: Float
    where trimmedDigit = [x | x <- string, isDigit x || x == '.']


-- Data types --

-- data       - Datatyp. L är typnamn och R är "type constructors"
--            - type Day = Monday | Tuesday ... deriving (Show)

-- type       - Alias, e.g. type String = [Char]. read
--            - Kan användas i type declaration för funktioner
--            - Fungerar för read. E.x. read "(1,1)" :: Point  om type Point = (Int, Int)

-- class      - Deklarerar "type classes". Ex. Num, Functor, Eq, Show. Typ "interface"
--            - class Eq a where
--                 (==) :: a -> a -> Bool
--                 (/=) :: a -> a -> Bool

-- instance   - Implementationer av typklasser för datatyper. När deriving (x) inte gör jobbet
--            - instance Functor Tree where
--            -     fmap f EmptyTree = EmptyTree
--            -     fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)

-- newtype    - Skapar en ny typ av en befintlig. Är typ en "container". Eg. ZipList
--            - newtype Ziplist [] = Ziplist {getZipList :: []}
--            - Används för att kunna skapa olika beteenden för samma underliggande datatyp.
--            - Te.x. kan nummer vara monoids på olika sätt 1 + (1 + 0) == (1 + 1) + 0
--            - eller 1 * (2 * 2) == (1 * 2) * 2
--            - För detta endamål skapades "newtypes" för Sum resp Product

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r * r
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (x2 - x1) * (y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) rad) offsetX offsetY = Circle (Point (x + offsetX) (y + offsetY)) rad
nudge (Rectangle (Point x1 y1) (Point x2 y2)) offsetX offsetY
    = Rectangle (Point (x1 + offsetX) (y1 + offsetY)) (Point (x2 + offsetX) (y2 + offsetY))

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Show)
mikael = Person "Mikael" "Engström" 28

-- type fun

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

addNumber :: PhoneBook -> Name -> PhoneNumber -> PhoneBook
addNumber book name nr = book ++ [(name, nr)]

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
               deriving (Eq, Ord, Show, Read, Bounded, Enum)

firstDayOfWeek = minBound :: Weekday
allWeekDays = [minBound .. maxBound] :: [Weekday]

-- Recursive data types

data List a = Empty | Cons a (List a)
    deriving (Show)

myList = Cons 1 (Cons 2 (Cons 3 Empty))

-- Binary search tree

data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Show, Read, Eq)

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)

instance Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = foldMap f l `mappend`
                             f x `mappend`
                             foldMap f r

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x EmptyTree = singleton x
insertTree x (Node a left right)
    | x == a = Node a left right
    | x < a  = Node a (insertTree x left) right
    | x > a  = Node a left (insertTree x right)

tree = foldr insertTree EmptyTree [1,2,23, 5,2,62, 6,78,1,23,14,5123,42,32]

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem nail (Node val left right)
    | nail == val = True
    | nail < val  = treeElem nail left
    | nail > val  = treeElem nail right



-- data Maybe a = Nothing | Just a --

strPos :: Char -> [Char] -> Maybe Int
strPos nail xs = search 0 xs
    where search _ [] = Nothing
          search i (x:xs) = if x == nail then Just i else search (i + 1) xs

printStringPosition :: Char -> [Char] -> IO ()
printStringPosition nail haystack = putStrLn outputString
    where outputString = case (strPos nail haystack) of (Nothing) -> "Not found"
                                                        (Just pos) -> "String found at position: " ++ (show pos)

-- Custom typeclasses --

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno True = True
    yesno _ = False

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _ = True


-- Some IO --

readAndPrint = do
    line <- getLine
    let line' = reverse line
    putStrLn $ line ++ " is " ++ line' ++ " reversed."
    putStrLn $ if (line == line') then "Its a palindrome" else "Its not a palindrome"

readAndPrint' = do
    line <- fmap reverse getLine
    putStrLn $ "You said " ++ line ++ " backwards."

shout = do line <- fmap ((++ "!!!") . map toUpper) getLine
           putStrLn line

-- Applicative functors --

coolStuff = [(+10), (*2), (**2)] <*> [1,2,3]

shoutVersions x = [(++ "!"), (map toUpper), ((++ "!!!") . map toUpper)] <*> [x]
-- ["Mikael!","MIKAEL","MIKAEL!!!"]

justMath = (+) <$> Just 10 <*> Just 20
-- Just 30

addJust :: (Num a) => Maybe a -> Maybe a -> Maybe a
addJust x y = (+) <$> x <*> y

addJust' :: (Num a) => Maybe a -> Maybe a -> Maybe a
addJust' x y = pure (+) <*> x <*> y

pow1pow2pow3 :: Double -> [Double]
pow1pow2pow3 = (\x y z -> [x, y, z]) <$> (**1) <*> (**2) <*> (**3)

cartesian :: [a] -> [b] -> [(a, b)]
cartesian x y = (\x y -> (x, y)) <$> x <*> y


-- Monads ---

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

--- *Main Data.Monoid F> applyMaybe (Just 10) (\x -> Just $ x + 10)
--- Just 20
--- *Main Data.Monoid F> Just 3 `applyMaybe` \x -> Just(x + 19)
--- Just 22
--- *Main Data.Monoid F> Just 3 `applyMaybe` \x -> Just(x + 19)
--- Just 22
--- *Main Data.Monoid F> Just 3 `applyMaybe` \x -> Just(x + 123)
--- Just 126
--- *Main Data.Monoid F> Just "hej" `applyMaybe` \x -> Just(x ++ "hej")

addIfEven :: (Integral a) => a -> a -> Maybe a
addIfEven x i
    | even x = Just $ i + x
    | otherwise = Nothing

-- *Main> addIfEven 2 2 >>= addIfEven 3 >>= addIfEven 4
-- Nothing
-- *Main> addIfEven 2 2 >>= addIfEven 2 >>= addIfEven 4
-- Just 10
-- *Main> Just 10 >> Nothing >> Just 20
-- Nothing

concatMaybe :: Maybe String -> Maybe String -> Maybe String
concatMaybe a b = do
    x <- a
    y <- b
    Just $ x ++ y

-- *Main> concatMaybe (Just "hej ") (Just "!")
-- Just "hej !"

-- TODO , implementera "knightMoveIn3 Pos Pos"
