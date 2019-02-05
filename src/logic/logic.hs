import System.IO
import Data.List.Unique
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

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
l_eval (Impl_Op l r) f = (not $ l_eval l f) || l_eval r f

newtype Log_Table = Log_Table (L_Op, [Char])

instance Show Log_Table where
  show (Log_Table a) = h ++ "\n" ++ rows
    where op = fst a
          ca = snd a
          h = "|" ++ (concat [c : "     |" | c <- ca]) ++ "res"
          caf = char_fun ca
          rows = (concat $ ["|" ++ (concat ([(show $ head $ show $ f c) ++ "   |" | c <- ca])) ++ (show $ l_eval op f) ++ "\n" | f <- caf])

get_tup :: Log_Table -> (L_Op, [Char])
get_tup (Log_Table a) = a

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

-- util

rem_dups :: [Char] -> [Char]
rem_dups = rd_helper []
  where rd_helper seen [] = seen
        rd_helper seen (x:xs)
          | x `elem` seen = rd_helper seen xs
          | otherwise = rd_helper (seen ++ [x]) xs

-- lexer

language_def =
  emptyDef { Token.identStart = letter
           , Token.identLetter = alphaNum
           , Token.reservedOpNames = [ "and", "or"
                                     ]
           }

lexer = Token.makeTokenParser language_def

identifier  = Token.identifier lexer
reserved_op = Token.reservedOp lexer
parens      = Token.parens     lexer
white_space = Token.whiteSpace lexer

l_exp :: Parser Log_Table
l_exp = do
  white_space
  res <- lop
  return (Log_Table ((fst $ get_tup res), rem_dups $ snd $ get_tup res))

lop :: Parser Log_Table
lop = parens lop
  <|> lop'

lop' :: Parser Log_Table
lop' = and_op
  <|> or_op
  <|> not_op
  <|> impl_op
  <|> var_op

and_op :: Parser Log_Table
and_op = do
  reserved_op "and"
  vl <- lop
  vr <- lop
  return (Log_Table (And_Op (fst $ get_tup vl) ((fst $ get_tup vr)), (snd $ get_tup vl) ++ (snd $ get_tup vr)))

or_op :: Parser Log_Table
or_op  = do
  reserved_op "or"
  vl <- lop
  vr <- lop
  return (Log_Table (Or_Op (fst $ get_tup vl) ((fst $ get_tup vr)), (snd $ get_tup vl) ++ (snd $ get_tup vr)))

impl_op :: Parser Log_Table
impl_op  = do
  reserved_op "impl"
  vl <- lop
  vr <- lop
  return (Log_Table (Impl_Op (fst $ get_tup vl) ((fst $ get_tup vr)), (snd $ get_tup vl) ++ (snd $ get_tup vr)))

not_op :: Parser Log_Table
not_op = do
  reserved_op "not"
  v <- lop
  return (Log_Table (Not_Op (fst $ get_tup v), snd $ get_tup v))

var_op :: Parser Log_Table
var_op = do
  i <- identifier
  return (Log_Table ((Op (head i)), [head i]))

parse_l :: String -> Log_Table
parse_l input =
  case parse l_exp "(unknown)" input of
    Left e  -> error $ show e
    Right r -> r

parse_user_input :: IO ()
parse_user_input = do
  input <- getLine
  if input == "q"
    then return ()
    else do
    print $ parse_l input
    parse_user_input
    
main = do
  parse_user_input
