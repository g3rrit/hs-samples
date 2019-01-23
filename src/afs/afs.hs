import Data.List

type X = [Char]
type Q = [Int]
type T = Char -> [(Int,Int)]
type I = [Int]
type F = [Int]

data Automata = NFA X Q T I F

-- | HELPER
is_not_elem :: (Eq x) => [x] -> x -> Bool
is_not_elem a b = not $ elem b a

lst_fst :: [(a,b)] -> [a]
lst_fst x = map fst x

-- | checks if left list is subset of right
is_subset :: (Eq x) => [x] -> [x] -> Bool
is_subset b a = null $ filter (is_not_elem a) b

create_nfa :: X -> Q -> T -> I -> F -> Automata
create_nfa x q t i f | not $ is_subset i q = error "initials are no subset of states"
                     | not $ is_subset f q = error "finals are no subset of states"
                     | otherwise =  NFA x q t i f

-- | getter
get_t :: Automata -> T
get_t (NFA _ _ t _ _) = t

transition :: Automata -> Char -> [(Int, Int)]
transition a b = get_t a $ b

is_dfa :: Automata -> Bool
is_dfa (NFA x q t i f) | length f /= 1 = False
                       | all (\xe -> sort (lst_fst(t xe)) == sort q) x = True
                       | otherwise = False

nfa_to_dfa :: Automata -> Automata
nfa_to_dfa (NFA x q t i f) | is_dfa $ NFA x q t i f = NFA x q t i f
                           | otherwise = do
                               let i_n = head $ [1..] \\ q

test = create_nfa ['a','b'] [1, 2] (\a -> case a of
                                          'a' -> [(1,1), (2, 2)]
                                          'b' -> [(2,2), (1,1)])
       [1] [2]


