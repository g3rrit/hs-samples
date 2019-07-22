
type Error = String
newtype Parser t a = Parser ([t] -> ([t], Either Error a))

instance Functor (Parser t) where
  fmap f (Parser p) = Parser $ \s -> case p s of
    (r, Left e)  -> (r, Left e)
    (r, Right a) -> (r, Right (f a))

instance Applicative (Parser t) where
  pure a = Parser (\s -> (s, Right a))
  (<*>) (Parser f0) (Parser f1) = Parser $ \s -> case f0 s of
    (r0, Left e)  -> (r0, Left e)
    (r0, Right f) -> case f1 r0 of
      (r1, Left e)   -> (r1, Left e)
      (r1, Right a1) -> (r1, Right (f a1))

instance Monad (Parser t) where
  return a = Parser $ \s -> (s, Right a)
  (>>=) (Parser p) f = Parser $ \s -> case p s of
    (r, Left e)  -> (r, Left e)
    (r, Right a) -> let Parser p = f a in p r
