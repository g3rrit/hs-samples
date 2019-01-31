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


-- get character table for evaluation
char_row :: [Char] -> Int -> [(Char, Bool)]
char_row c i 
  | s < 0 = []
  | otherwise = [(cc, t)] ++ char_row (drop 1 c) (if t then res else i)
    where s = (length c) - 1
          cc = head c
          res = i - 2^s
          t = res >= 0

char_table' :: [Char] -> Int -> [[(Char, Bool)]]
char_table' a i 
  | i < 0 = []
  | otherwise = [(char_row a i)] ++ (char_table' a (i - 1))
                         
char_table :: [Char] -> [[(Char, Bool)]]
char_table a = char_table' a (2^(length a) - 1)

c_lookup :: [(Char, Bool)] -> Char -> Bool
c_lookup ca c = case lookup c ca of
                  Just n -> n
                  Nothing -> False

char_fun :: [Char] -> [(Char -> Bool)]
char_fun ca = map (\c -> (c_lookup c)) ra
  where ra = char_table ca 
--------------------------------------


test = And_Op (Op 'a') (Op 'a')
res = l_eval test (\a -> case a of
                           'a' -> True
                           'b' -> False)

-- parser
l_exp :: GenParser Char st (L_Op, [Char])
l_exp = do
  r <- lop
  eol
  return r

eol :: GenParser Char st Char
eol = char '\n'

lop :: GenParser Char st (L_Op, [Char])
lop = and_op

and_op :: GenParser Char st (L_Op, [Char])
and_op = do
  string "and"
  r <- tuple
  return r

tuple :: GenParser Char st (L_Op, [Char])
tuple = do
  char '('
  l <- var_char
  char ','
  r <- var_char
  char ')'
  return ((And_Op (Op l) (Op r)), [l] ++ [r])

var_char :: GenParser Char st Char
var_char = do
  c <- anyChar
  return c

parse_l :: String -> Either ParseError (L_Op, [Char])
parse_l input = parse l_exp "(unknown)" input
  
get_either_dummy :: Either ParseError (L_Op, [Char]) -> (L_Op, [Char])
get_either_dummy (Left a) = (Op 'a', ['a'])
get_either_dummy (Right a) = a

main = do
  res <- get_either_dummy $ parse_l "and(a b)"
  op <- fst res
  ca <- snd res
  cfa <- char_fun $ ca
  a <- concat [(show (l_eval (op fun)))  ++ "\n" | fun <- cfa]
  putStr a
