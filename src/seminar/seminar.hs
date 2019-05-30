

data Tree a = Leaf
            | Branch (Tree a) a (Tree a)


{-
odd = \a -> a | a `mod` 2 == 0 -> False
              | a `mod` 2 == 1 -> True
-}

{-

odd a | a `mod` 2 == 0 = False
      | otherwise = True


foo :: IO String
foo = do
  res <- getLine
  return res

{-

bar = let a = 10
          b = 10
      in
        a * b

-}

bar = let { a = 10 ;b = 20 } in a * b

-}

testm = Just 10

foo a = return (a * a)
