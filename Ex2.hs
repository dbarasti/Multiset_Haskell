module Ex2 where

    import Ex1

    instance Foldable ListBag where
        foldr _ zero empty = zero
        foldr f zero (LB ((a, _):rest)) =
            f a (foldr f zero (LB rest))


    doMap :: Eq a => (a -> b) -> ListBag a  -> ListBag b
    doMap _ (LB []) = empty
    doMap f (LB ((x, y):rest)) = pushFront ((f x) , y) (mapLB  f (LB rest))


    mapLB :: Eq a => (a -> b) -> ListBag a  -> ListBag b
    mapLB f (LB bag)
            | wf (LB bag) = doMap f (LB bag)
            | otherwise = doMap f (doWellForm (LB bag))
            
-- It is not possible to define an istance of Functor for 
-- ListBag providing mapLB as implementation due to the mandatory type 'Eq a' 
-- required by mapLB. Infact, since the implementation of fmap in Functor does not
-- require it, there is a type mismatch.  
-- 
