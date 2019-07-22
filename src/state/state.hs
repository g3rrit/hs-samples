data State s a = State (s -> (a, s))

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put x = State (\s -> ((), x))

eval :: State s a -> s -> a
eval (State s) a = let (r, _) = s a in r

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  (>>=) (State act) f = State $ \s ->
    let (a, s') = act s
        State n = f a
    in n s'

instance Functor (State s) where
  fmap f (State act) = State $ \s ->
    let (a, s') = act s
    in (f a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (<*>) (State a0) (State a1) = State $ \s ->
    let (f , s0') = a0 s
        (a1', s1') = a1 s0'
    in (f a1', s1')

inc = do
  c <- get
  put $ c + 1

main = print $ eval (do inc ;inc ;get) 0
