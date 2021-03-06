{-# LANGUAGE FlexibleInstances #-}
-- | This module provides an interpretator for the LOOP programming language.
module Loop where


-- * Environment

type Var = String
type Val = Int
type Env = Var -> Val

fetch :: Var -> Env -> Val
fetch v env = env v

update :: Var -> Val -> Env -> Env
update v i env v' = if v' == v then i else env v'


-- * Expressions

type Exp = Env -> Val

class IsExp e      where toExp :: e -> Exp
instance IsExp Val where toExp = const
instance IsExp Var where toExp = fetch
instance IsExp Exp where toExp = id

bop :: (IsExp e1, IsExp e2) => (Val -> Val -> Val) -> e1 -> e2 -> Exp
bop op x y env = toExp x env `op` toExp y env

plus, mult :: (IsExp e1, IsExp e2) => e1 -> e2 -> Exp
plus = bop (+)
mult = bop (*)


-- * Deterministic Statements

type P   = Env -> [Env]

skip :: P
skip env = [env]

assign :: IsExp e1 => Var -> e1 -> P
assign v e env = [update v (toExp e env) env]

sequ :: P -> P -> P
sequ p1 p2 env = p1 env >>= p2

-- loop X P == P^X
loop :: Var -> P -> P
loop v p env = iterate (concatMap p) [env] !! fetch  v env


-- * Non-deterministic Statements

choose :: P -> P -> P
choose p1 p2 env = p1 env ++ p2 env

-- nasign X E == X \in [0..E]
nassign :: IsExp e1 => Var -> e1 -> P
nassign v e env = [ update v i env | i <- [0..toExp e env] ]

-- nloop X P == [skip, P^1,...,P^X]
nloop :: Var -> P -> P
nloop v p env = concat $ take (fetch v env + 1) $ iterate (concatMap p) [env]


-- * Evaluator

eval :: P -> [(Var,Val)] -> [Env]
eval p = p . foldr (uncurry update) (\v -> error $ "not defined" ++ v )


-- * Syntactic Sugar

infix 6 .+,.*
infix 5 .=
infix 4 <|>

(.+), (.*) :: (IsExp e1, IsExp e2) => e1 -> e2 -> Exp
(.+) = plus
(.*) = mult

(.=),(.~) :: IsExp e1 => Var -> e1 -> P
(.=) = assign
(.~) = nassign

(<|>) :: P -> P -> P
(<|>) = choose

(<~) :: a -> b -> (a,b)
(<~) = (,)


-- * Pretty Printing

ppl :: [(Var, Val)] -> String
ppl = foldr k "" where k (v,i) acc = v ++ " = " ++ show i ++ "\t" ++ acc

pretty :: [Var] -> [Env] -> String
pretty vs = unlines . map k where k env = "  " ++ ppl [ (v,i) | v <- vs, let i = fetch v env ]

