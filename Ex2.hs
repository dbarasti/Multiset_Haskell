module Ex2 where

    import Ex1

    -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
    instance Foldable ListBag where
        foldr f z (LB []) = z
        foldr f z (LB ((a, i):lb)) =
            f a (foldr f z (LB lb))

    mapLB :: (a -> b) -> ListBag a  -> ListBag b
    mapLB _ (LB []) = empty
    mapLB f (LB ((x, y):rest)) = pushFront ((f x) , y) (mapLB  f (LB rest))
            
            
