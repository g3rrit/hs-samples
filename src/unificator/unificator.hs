import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.Foldable as F

data Term = Fn Char [Term] | Var Char deriving Show

instance Eq Term  where
  (==) a b = case (a, b) of
               (Fn sn st, Fn tn tt) -> (sn == tn) && ((length st) == (length tt)) && and (map (\(a,b) -> a == b) (zip st tt))
               (Var sn, Var tn)     -> (sn == tn)
               (_ , _)              -> False

instance Ord Term where
  compare a b = if a == b then EQ else case (a, b) of
                                         (Var _, Fn _ _) -> LT
                                         (Fn _ _, Var _) -> GT
                                         (Fn sn _, Fn tn _) -> compare sn tn
                                         (Var sn, Var tn) -> compare sn tn

data Rule = Rule Char Term deriving Show

instance Eq Rule where
  (==) (Rule a b) (Rule x y) = a == x

instance Ord Rule where
  compare (Rule a b) (Rule x y) = compare a x

type Unificator = Set.Set Rule


-- -------- UNIFICATOR_ALG-------- --

vars :: Term -> Set.Set Char
vars t = case t of
           Fn n t' -> foldr (\a b -> Set.union (vars a) b) Set.empty t'
           Var c -> Set.singleton c

compare_arity_sym :: Term -> Term -> Bool
compare_arity_sym (Fn sn st) (Fn tn tt) = (sn == tn) && ((length st) == (length tt))
compare_arity_sym _ _ = False

get_unificator :: Term -> Term -> Maybe Unificator
get_unificator s t = if s == t then Just (Set.empty) else
              case (s, t) of
                (Var sn, _)          -> if (Set.member sn (vars t)) then Nothing else Just (Set.singleton (Rule sn t))
                (_, Var _)           -> get_unificator t s
                (Fn sn st, Fn tn tt) -> if (compare_arity_sym s t) then foldr (\(a, b) x -> uadd a b x) (Just Set.empty) (zip st tt) else Nothing
  where uadd l m x = do
          x' <- x
          h  <- get_unificator (apply_unificator l x') (apply_unificator m x')
          return (Set.union x' h)

apply_unificator :: Term -> Unificator -> Term
apply_unificator t u = case t of
  Fn n tx -> Fn n (map (`apply_unificator` u) tx)
  Var n -> let r = (F.find (\(Rule a b) -> a == n) u) in
             case r of
               Nothing -> Var n
               Just (Rule a b) -> b

-- -------- PARSER-------- --

parse_term :: String -> Maybe (Term, String)
parse_term [] = Nothing
parse_term (x:xs) = case x of
  a | Char.isAlpha a -> Just (Var a, xs)
  '(' -> parse_fn xs
  ' ' -> parse_term xs
  _ -> Nothing

parse_fn :: String -> Maybe (Term, String)
parse_fn [] = Nothing
parse_fn (x:xs) = case x of
  a | Char.isAlpha a -> let args = parse_fn_args xs in
                     case args of
                       Just (args', xs') -> Just (Fn a args', xs')
                       Nothing -> Nothing
  ' ' -> parse_fn xs
  _ -> Nothing

parse_fn_args :: String -> Maybe ([Term], String)
parse_fn_args [] = Nothing
parse_fn_args (x:xs) = case x of
  x | x == ')' -> Just ([], xs)
  x | x == ' ' -> parse_fn_args xs
  _ -> let r = parse_term (x:xs) in
         case r of
           Just (t, xs') -> let rs = parse_fn_args xs' in
                              case rs of
                                Just (rs', xss) -> Just (t : rs', xss)
                                Nothing -> Nothing
           Nothing -> Nothing

-- -------- MAIN-------- --

main = do
  putStr "Enter term s: "
  s <- getLine
  putStr "Enter term t: "
  t <- getLine
  case (parse_term s, parse_term t) of
    (Just (s', _), Just (t', _)) -> do
      putStrLn ("s : (" ++ (show s') ++ ")")
      putStrLn ("t : (" ++ (show t') ++ ")")
      putStrLn ("unificator: " ++ (show (get_unificator s' t')))
    _ -> putStrLn "Error on input"
