{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

data Chain a = Head a | Tail (a -> Chain a)

class Chainable r a where
   chain :: r -> Chain a

instance Chainable a a where
  chain = Head

instance Chainable r a => Chainable (a -> r) a where
  chain f = Tail $ \v -> chain $ f v

feed :: Chain a -> a -> Chain a
feed (Tail f) v = f v
feed c _ = c

unChain :: Chain a -> Maybe a
unChain (Head v) = Just v
unChain _ = Nothing
