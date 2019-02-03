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

newtype Log_Table = Log_Table (L_Op, [Char])

instance Show Log_Table where
  show (Log_Table a) = h ++ "\n" ++ rows
    where op = fst a
          ca = snd a
          h = "|" ++ (concat [c : "     |" | c <- ca]) ++ "res"
          caf = char_fun ca
          rows = "|" ++ (concat $ [(concat ([(show $ f c) ++ " |" | c <- ca])) ++ (show $ l_eval op f) ++ "\n" | f <- caf])

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

-- parser
l_exp :: GenParser Char st Log_Table
l_exp = do
  spaces
  r <- lop
  spaces
  eol
  return $ (Log_Table r)

eol :: GenParser Char st Char
eol = char '\n'

lop :: GenParser Char st (L_Op, [Char])
lop = and_op <|> var_op

and_op :: GenParser Char st (L_Op, [Char])
and_op = do
  l <- lop
  spaces
  string "and"
  spaces
  r <- lop
  return (And_Op (fst l) (fst r), (snd l) ++ (snd r))

var_op :: GenParser Char st (L_Op, [Char])
var_op = do
  c <- anyChar
  return ((Op c), [c])


parse_l :: String -> Either ParseError Log_Table
parse_l input = parse l_exp "(unknown)" input

  
get_either_dummy :: Either ParseError Log_Table -> Log_Table
get_either_dummy (Left a) = Log_Table (Or_Op (Op 'a') (Op 'b'), ['a', 'b'])
get_either_dummy (Right a) = a

main = do
  let res = get_either_dummy $ parse_l "a and b\n"
  print (Log_Table (Or_Op (Op 'a') (Op 'b'), ['a', 'b']) )
  print res
