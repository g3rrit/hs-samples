import qualified Data.Set as Set

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

type Unificator = Set.Set (Char, Term)

vars :: Term -> Set.Set Char
vars t = case t of
           Fn n t' -> foldr (\a b -> Set.union (vars a) b) Set.empty t'
           Var c -> Set.singleton c

compare_arity_sym :: Term -> Term -> Bool
compare_arity_sym (Fn sn st) (Fn tn tt) = (sn == tn) && ((length st) == (length tt))
compare_arity_sym _ _ = False

unify :: Term -> Term -> Maybe Unificator
unify s t = if s == t then Just (Set.empty) else
              case (s, t) of
                (Var sn, _)          -> if (Set.member sn (vars t)) then Nothing else Just (Set.singleton (sn, t))
                (_, Var _)           -> unify t s
                (Fn sn st, Fn tn tt) -> if (compare_arity_sym s t) then foldr (\(a, b) x -> uadd a b x) (Just Set.empty) (zip st tt) else Nothing
  where uadd l m x = do
          x' <- x
          h  <- unify l m
          return (Set.union x' h)
