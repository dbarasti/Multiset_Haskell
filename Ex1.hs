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
) where

import Data.Maybe

data ListBag a = LB [(a, Int)] 
  deriving (Show, Eq)

empty = LB []

singleton v = LB [(v,1)]

fromList :: Eq a => [a] -> ListBag a
fromList [x] = add x empty
fromList (x:xs) = add x (fromList xs)

search :: Eq a => a -> [(a,Int)] -> Maybe (a, Int)
search _ [] = Nothing 
search a ((x,y):rest) | a == x = Just (x,y)
                      | otherwise = search a rest

searchLB :: Eq a => a -> ListBag a -> Maybe (a, Int)
searchLB a (LB x) = search a x 
  

pushFront :: (a, Int) -> ListBag a -> ListBag a 
pushFront (x,y) (LB z)= LB ([(x,y)] ++ z)

add :: Eq a => a -> ListBag a -> ListBag a
add a (LB ((x, y):rest))
      | a == x =  LB ((x, y + 1):rest)
      | otherwise = pushFront (x, y) (add a (LB rest))
add a (LB []) = singleton a


wf :: Eq a => ListBag a -> Bool
wf (LB ((x,y):rest)) 
      | (search x rest) == Nothing = wf(LB rest)
      | otherwise = False
wf (LB []) = True

isEmpty :: ListBag a -> Bool
isEmpty (LB []) = True
isEmpty (LB _) = False

mul :: Eq a => a -> ListBag a -> Int
mul v bag
      | result == Nothing = 0
      | otherwise = snd (fromJust result)
      where result = searchLB v bag


listify :: (a, Int) -> [a]
listify (x,y) 
      | y == 0 = []
      | otherwise = x : listify (x, y-1)

toList :: ListBag a -> [a]
toList (LB ((x,y):rest)) = (listify (x,y)) ++ (toList (LB rest))
toList (LB []) = []  

addTuple :: Eq a => (a, Int) -> ListBag a -> ListBag a 
addTuple t (LB ((x,y):rest))
      | (fst t) == x = LB((x,y + (snd t)):rest)
      | otherwise = pushFront (x, y) (addTuple t (LB rest))
addTuple t (LB []) = LB [t]      

sumBag :: Eq a => ListBag a -> ListBag a -> ListBag a 
sumBag bag (LB [y]) =  addTuple y bag
sumBag bag (LB (y:ys)) = sumBag (addTuple y bag) (LB ys)

