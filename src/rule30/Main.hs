{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

rule :: Bool -> Bool -> Bool -> Bool
rule True True True    = False
rule True True False   = False
rule True False True   = False
rule True False False  = True
rule False True True   = True
rule False True False  = True
rule False False True  = True
rule False False False = False

nl :: [Bool] -> [Bool]
nl l = nl' False False $ l ++ [False, False]
  where nl' b0 b1 (b:bs) = (rule b0 b1 b) : nl' b1 b bs
        nl' _ _ [] = []
  
grid :: [[Bool]]
grid = [True] : grid' [True]
  where grid' l = let r = nl l in r : grid' r

binToDec :: [Bool] -> Integer
binToDec = foldr (\a b -> 2 * b + case a of { True -> 1 ;False -> 0}) 0

toNum :: [[Bool]] -> [Integer]
toNum = map binToDec

prime_factors n =
  case factors of
    [] -> [n]
    _  -> factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]
