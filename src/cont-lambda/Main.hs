-- 
-- continuation passing style example
-- taken from:
-- https://kseo.github.io/posts/2017-01-09-continuation-passing-style-interpreter.html`
--

data Expr
  = Var Int
  | Lam Expr
  | App Expr Expr
  | Escape Expr
  | Lit Int
  | Prim PrimOp Expr Expr
  deriving Show

data PrimOp = Add | Mul
  deriving Show

type Cont = Value -> Value

data Value
  = VInt Int
  | VClosure (Value -> Cont -> Value)

instance Show Value where
  show (VInt i) = show i
  show VClosure{} = "<<closure>>"

type Env = [Value]

eval :: Env -> Expr -> Cont -> Value
eval env term k = case term of
  Var n -> k $ env !! n
  Lam a -> k $ VClosure (\v k' -> eval (v : env) a k')
  App a b ->
    eval env a $ \(VClosure c) ->
    eval env b $ \v ->
    c v k
  Escape a -> eval (VClosure (\v _ -> k v) : env) a k
  Lit n -> k $ VInt n
  Prim p a b -> eval env a $ \v1 ->
                eval env b $ \v2 ->
                k $ evalPrim p v1 v2

evalPrim :: PrimOp -> Value -> Value -> Value
evalPrim Add (VInt a) (VInt b) = VInt (a + b)
evalPrim Mul (VInt a) (VInt b) = VInt (a * b)

emptyEnv :: Env
emptyEnv = []

evalExpr :: Expr -> Value
evalExpr e = eval emptyEnv e id
