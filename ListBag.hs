module ListBag where

import Data.Maybe

data ListBag a = ListBag [(a, Int)] 
  deriving (Show, Eq)

empty = ListBag []

singleton v = ListBag [(v,1)]

fromList :: Eq a => [a] -> ListBag a
fromList [x] = add x empty
fromList (x:xs) = add x (fromList xs)

search :: Eq a => a -> [(a,Int)] -> Maybe (a, Int)
search _ [] = Nothing 
search a ((x,y):rest) | a == x = Just (x,y)
                      | otherwise = search a rest

searchLB :: Eq a => a -> ListBag a -> Maybe (a, Int)
searchLB a (ListBag x) = search a x 
  

pushFront :: (a, Int) -> ListBag a -> ListBag a 
pushFront (x,y) (ListBag z)= ListBag ([(x,y)] ++ z)

add :: Eq a => a -> ListBag a -> ListBag a
add a (ListBag ((x, y):rest))
      | a == x =  ListBag ((x, y + 1):rest)
      | otherwise = pushFront (x, y) (add a (ListBag rest))
add a (ListBag []) = singleton a


wf :: Eq a => ListBag a -> Bool
wf (ListBag ((x,y):rest)) 
      | (search x rest) == Nothing = wf(ListBag rest)
      | otherwise = False
wf (ListBag []) = True

isEmpty :: ListBag a -> Bool
isEmpty (ListBag []) = True
isEmpty (ListBag _) = False

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
toList (ListBag ((x,y):rest)) = (listify (x,y)) ++ (toList (ListBag rest))
toList (ListBag []) = []  

addTuple :: Eq a => (a, Int) -> ListBag a -> ListBag a 
addTuple t (ListBag ((x,y):rest))
      | (fst t) == x = ListBag((x,y + (snd t)):rest)
      | otherwise = pushFront (x, y) (addTuple t (ListBag rest))
addTuple t (ListBag []) = ListBag [t]      

sumBag :: Eq a => ListBag a -> ListBag a -> ListBag a 
sumBag bag (ListBag [y]) =  addTuple y bag
sumBag bag (ListBag (y:ys)) = sumBag (addTuple y bag) (ListBag ys)


mapLB :: (a -> b) -> ListBag a  -> ListBag b
mapLB _ (ListBag []) = empty
mapLB f (ListBag ((x, y):rest)) = pushFront ((f x) , y) (mapLB  f (ListBag rest))

foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
instance Foldable ListBag where 
      foldr f z (LB []) = z
      foldr f z (LB ((a, i):lb)) =
            f a (foldr f z (LB lb))




