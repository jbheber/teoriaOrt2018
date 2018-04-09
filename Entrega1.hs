--Entrega 1 Juan Bautista Heber
module Entrega1 where
import Prelude hiding ((++), reverse)

-- Ejercicio 1
-- a)
data N = O | Suc N
      deriving (Show, Eq)

-- b)
suma :: N -> N -> N
suma O x = x
suma (Suc a) b = Suc (suma a b)

--Explicacion
--Termina para cualquier entrada porque por pattern matching, 
--se describen casos exhaustivos y suma va reduciendo el primer término
--hasta que coincide con el caso base

-- c)
sub :: N -> N -> N
sub x O = x
sub O (Suc x) = O
sub (Suc a) (Suc b) = sub a b

--Explicacion
--Termina para cualquier entrada porque va reduciendo ambos términos
--hasta que coincida con el caso base.

-- d)
-- x >= y
graterEq :: N -> N -> Bool
graterEq = \x -> \y -> case y of {
 O -> True;
 Suc z ->  case x of {
    O -> False;
    Suc t -> graterEq t z;
  };
}; 

cosc :: N -> N -> N
cosc = \n -> \d -> case d of {
 O -> undefined;
 x -> case n of {
    O -> O;
    y -> case (graterEq n d) of {
       False -> O;
       True -> Suc (cosc (sub n d) d);
      };
    };
};

--cosciente :: N -> N -> N
--cosciente m n
--   | graterEq n m = O
--   | otherwise = Suc (cosciente (sub m n) n)
--Explicacion
--Termina cuando el denominador es 0 ya que el cociente de un numero y 0 es infinito
--Termina cuando el numero es menor al denominador ya que se le va substrayendo el valor del denominador al numero

-- Ejercicio 2
-- a)
(++) :: [a] -> [a] -> [a]
(++) = \xs -> \ys -> case xs of {
   [] -> ys;
   (z:zs) -> z:(zs ++ ys); 
};

-- b)
reverse :: [a] -> [a]
reverse = \xs -> case xs of {
  [] -> [];
  (z:zs) -> (reverse zs) ++ [z];
} 
rev1 :: [a] -> [a]
rev1 [] = []
rev1 (x:xs) = rev1 xs ++ [x]

-- c)
rev2 :: [a] -> [a] -> [a]
rev2 acc [] = acc
rev2 acc (x:xs) = rev2 (x:acc) xs

-- d)
concatenar :: [[a]] -> [a]
concatenar = foldr (++) []

-- e)
reversef :: [a] -> [a]
reversef bs = foldr (\b g x -> g (b : x)) id bs []

-- f)
reversefl :: [a] -> [a]
reversefl xs = foldl (\acc x-> x : acc) [] xs

-- g)
veces :: Eq a => a -> [a] -> Int
veces x [] = 0;
veces x (y:ys)
    | y==x = 1 + (veces x ys)
    | otherwise = veces x ys

-- h)
expandir :: Eq a => [a] -> [(a, Int)] -> [(a, Int)]
expandir [] xs = xs
expandir (x:xs) ys = expandir (xs) (ys ++ [(x, 1 + (veces x (xs)))])

-- Ejercicio 3
data FProp = Const Bool
            | Var Char
            | Neg FProp
            | Conj FProp FProp
            | Impl FProp FProp
            deriving Show

valor :: Eq a => a -> [(a,v)] -> v
valor _ [] = undefined
valor x y = head [v | (x',v) <- y, x==x']

eval :: [(Char, Bool)] -> FProp -> Bool
eval _ (Const b) = b
eval m (Var p) = valor p m
eval val (Neg f) = not (eval val f)
eval val (Conj f1 f2) = (eval val f1) && (eval val f2)
eval val (Impl f1 f2) = (eval val f1) <= (eval val f2)
