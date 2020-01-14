import Data.List
data ListBag a = ListBag [(a, Int)]
  deriving (Show, Eq)

tailLB (ListBag x) = tail x
headLB (ListBag x) = head x
getLB (ListBag x) = x
search:: Eq a => a -> [(a,b)] -> Bool
search a ((x,y):rest) = if x == a
                          then True
                          else search a rest
search _ [] = False 

wf :: Eq a => ListBag a -> Bool
wf (ListBag ((x,y):rest)) = if (search x rest) == False
                  then   
                    wf(ListBag rest)
                  else False
wf (ListBag []) = True
