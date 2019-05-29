

data Foo a = A a deriving Show

instance Functor Foo where
  fmap f (A i) = A (f i)

main = do
  putStrLn "Starting"
  let foo = A 10
  let res = fmap (\a -> a * a) foo
  putStrLn ("res: " ++ (show res))
