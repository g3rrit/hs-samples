import Text.ParserCombinators.Parsec

data L_Op =
  Op Char
  | And_Op L_Op L_Op
  | Or_Op L_Op L_Op
  | Not_Op L_Op
  | Impl_Op L_Op L_Op

l_eval :: L_Op -> (Char -> Bool) -> Bool
l_eval (Op c) f = f c
l_eval (And_Op l r) f = l_eval l f && l_eval r f
l_eval (Or_Op l r) f = l_eval l f || l_eval r f
l_eval (Not_Op u) f = not $ l_eval u f
l_eval (Impl_Op l r) f = not $ l_eval l f && l_eval r f

test = And_Op (Op 'a') (Op 'a')
res = l_eval test (\a -> case a of
                           'a' -> True
                           'b' -> False)

exp :: GenParser Char st (L_Op, [Char])
exp = lop eol

lop :: GenParser Char st (L_Op, [Char])
lop = and_op <|> or_op <|> not_op <|> impl_op

and_op :: GenParser Char st {L_Op, [Char])
and_op = string "and" >> tuple


-- create_char_list :: [Char] -> [(Char, Bool)]



