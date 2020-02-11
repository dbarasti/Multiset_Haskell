module Ex1
( ListBag (LB)
, wf
, empty
, singleton
, fromList
, isEmpty
, mul
, toList
, sumBag
, pushFront
, doWellForm
) where

import Data.Maybe

data ListBag a = LB [(a, Int)]
  deriving (Show, Eq)

--the value constructor builds returns a well-formed listbag  
listBag :: Eq a => ListBag a -> ListBag a
listBag (LB x) 
            | wf (LB x) = (LB x)
            | otherwise = doWellForm (LB x)

empty = LB []

singleton v = LB [(v,1)]

--builds a ListBag from a list. It is inherently well-formed
fromList :: Eq a => [a] -> ListBag a
fromList [x] = add x empty
fromList (x:xs) = add x (fromList xs)

--looks for the first item of a tuple into an array. Uses Maybe type 
search :: Eq a => a -> [(a,Int)] -> Maybe (a, Int)
search _ [] = Nothing 
search a ((x,y):rest) | a == x = Just (x,y)
                      | otherwise = search a rest

--looks for an item in a ListBag. Uses the auxiliary function search
searchLB :: Eq a => a -> ListBag a -> Maybe (a, Int)
searchLB a (LB x) = search a x 
  

pushFront :: (a, Int) -> ListBag a -> ListBag a 
pushFront (x,y) (LB z)= LB ([(x,y)] ++ z)

--adds a value into an existing ListBag. Returns a well-formed ListBag
add :: Eq a => a -> ListBag a -> ListBag a
add a (LB ((x, y):rest))
      | a == x =  LB ((x, y + 1):rest)
      | otherwise = pushFront (x, y) (add a (LB rest))
add a (LB []) = singleton a


--checks wheather the ListBag is well-formed
wf :: Eq a => ListBag a -> Bool
wf (LB ((x,y):rest)) 
      | (search x rest) == Nothing = wf(LB rest)
      | otherwise = False
wf (LB []) = True


--makes a ListBag well-formed
doWellForm :: Eq a => ListBag a -> ListBag a
doWellForm (LB malformed) = sumBag (empty) (LB malformed)

isEmpty :: ListBag a -> Bool
isEmpty (LB []) = True
isEmpty (LB _) = False


--if present in the ListBag, returns the multeplicity of value v
mul :: Eq a => a -> ListBag a -> Int
mul v bag
      | searchResult == Nothing = 0
      | otherwise = snd (fromJust searchResult)
      where searchResult = searchLB v bag


--transforms a tuple of the form (value, multeplicity) into a list of n=multeplicity values.
listify :: (a, Int) -> [a]
listify (x,y) 
      | y == 0 = []
      | otherwise = x : listify (x, y-1)


--returns a listified Listbag. uses the auxiliary function listify
toList :: ListBag a -> [a]
toList (LB ((x,y):rest)) = (listify (x,y)) ++ (toList (LB rest))
toList (LB []) = []  

addTuple :: Eq a => (a, Int) -> ListBag a -> ListBag a 
addTuple t (LB ((x,y):rest))
      | (fst t) == x = LB((x,y + (snd t)):rest)
      | otherwise = pushFront (x, y) (addTuple t (LB rest))
addTuple t (LB []) = LB [t]      

--adds two ListBags, tuple by tuble
addBags :: Eq a => ListBag a -> ListBag a -> ListBag a 
addBags bag (LB [y]) =  addTuple y bag
addBags bag (LB (y:ys)) = addBags (addTuple y bag) (LB ys)


--ensures that the first ListBag is well-formed and summs togheter two Listbags. 
--Thanks to the implementation of addBags it is only necessary to check the first ListBag
sumBag :: Eq a => ListBag a -> ListBag a -> ListBag a
sumBag (LB bag1) (LB bag2)
            | wf(LB bag1) = addBags (LB bag1) (LB bag2)
            | otherwise = addBags (doWellForm (LB bag1)) (LB bag2)
