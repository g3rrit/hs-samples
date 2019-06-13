-- [ n | n element of naturals and primfactors of n are subset of {2, 3, 5}

merge (x:xs) (y:ys) = case compare x y of
                        LT -> x : merge xs (y:ys)
                        EQ -> x : merge xs ys
                        GT -> y : merge (x:xs) ys

hemming = 1 : merge (map (2*) h) (merge (map (3*) h) (map (5*) h))
