
data L_Op =
  Op Bool
  | And_Op L_Op L_Op
  | Or_Op L_Op L_Op
  | Not_Op L_Op

l_eval :: L_Op -> Bool
l_eval (Op b) = b
l_eval (And_Op l r) = l_eval l && l_eval r
l_eval (Or_Op l r) = l_eval l || l_eval r
l_eval (Not_Op u) = not $ l_eval u


test = Not_Op ( And_Op ( Op True) ( Op False))
