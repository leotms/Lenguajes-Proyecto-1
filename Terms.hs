{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Definicion de los terminos Term, Equation, y tipos para Sust.
-- Definicion de reglas de impresion.
-- Autores:
-- Aldrix Marfil     10-10940
-- Leonardo Martinez 11-10576

-- Para que sea un modulo importable
module Terms where

---------------------------------------------------------------
-- Definicion de Term

data Term =  Var String
        | Bool String
        | Or   Term Term
        | Neg  Term
        | And  Term Term
        | Imp  Term Term
        | Ioi  Term Term
        | Nioi Term Term
        deriving Eq

---------------------------------------------------------------
-- Definicion de Equation

data Equation = Eq Term Term

---------------------------------------------------------------
-- Definimos el alias para el tipo Sust

type Sust  = (Term,Term)
type Sust2 = (Term, Sust, Term)
type Sust3 = (Term, Term, Sust, Term, Term)


---------------------------------------------------------------
-- Funciones y Operadores

-- Booleanos
true :: Term
true = Bool "true"

false :: Term
false = Bool "false"

-- Or
(\/) :: Term -> Term -> Term
(\/) t1 t2 = Or t1 t2

-- And
(/\) :: Term -> Term -> Term
(/\) t1 t2 = And t1 t2

-- Not
neg :: Term -> Term
neg t1 = Neg t1

-- Implicacion
(==>) :: Term -> Term -> Term
(==>) t1 t2 = Imp t1 t2

-- Si y Solo Si
(<==>) :: Term -> Term -> Term
(<==>) t1 t2 = Ioi t1 t2

-- Inequivalente
(!<==>) :: Term -> Term -> Term
(!<==>) t1 t2 = Nioi t1 t2

-- Operador Equation
(===) :: Term -> Term -> Equation
(===) t1 t2 = Eq t1 t2

-- Operador Sustitucion
(=:) :: Term -> Term -> Sust
(=:) t1 t2 = (t1, t2)

---------------------------------------------------------------
-- Terminos para las variables del abecedario

a :: Term
a = Var "a"

b :: Term
b = Var "b"

c :: Term
c = Var "c"

d :: Term
d = Var "d"

e :: Term
e = Var "e"

f :: Term
f = Var "f"

g :: Term
g = Var "g"

h :: Term
h = Var "h"

i :: Term
i = Var "i"

j :: Term
j = Var "j"

k :: Term
k = Var "k"

l :: Term
l = Var "l"

m :: Term
m = Var "m"

n :: Term
n = Var "n"

o :: Term
o = Var "o"

p :: Term
p = Var "p"

q :: Term
q = Var "q"

r :: Term
r = Var "r"

s :: Term
s = Var "s"

t :: Term
t = Var "t"

u :: Term
u = Var "u"

v :: Term
v = Var "v"

w :: Term
w = Var "w"

x :: Term
x = Var "x"

y :: Term
y = Var "y"

z :: Term
z = Var "z"

---------------------------------------------------------------
-- Definimos la precedencia de los operadores

infixl 9 `neg`
infixl 3 /\
infixl 3 \/
infixr 2 ==>
infixl 1 <==>
infixl 1 !<==>
infixl 0 ===

---------------------------------------------------------------
-- Representacion en forma de String para los diferentes
-- tipos de datos.

-- Term --
showTerm :: Term -> String
showTerm (Var x) = x

showTerm (Bool x) = x

-- Expresiones con \/
showTerm (Or (Var x) (Var y)) = showTerm(Var x) ++ " \\/ " ++ showTerm(Var y)
showTerm (Or (Var x) term)    = showTerm(Var x) ++ " \\/ " ++ "(" ++ showTerm(term) ++ ")"
showTerm (Or term (Var x))    = "(" ++ showTerm(term) ++ ")" ++ " \\/ " ++ showTerm(Var x)
showTerm (Or term1 term2)     = "(" ++ showTerm(term1) ++ ")" ++ " \\/ " ++ "(" ++ showTerm(term2) ++ ")"

-- Expresiones con /\
showTerm (And (Var x) (Var y)) = showTerm(Var x) ++ " /\\ " ++ showTerm(Var y)
showTerm (And (Var x) term)    = showTerm(Var x) ++ " /\\ (" ++ showTerm(term) ++ ")"
showTerm (And term (Var x))    = "(" ++ showTerm(term) ++ ") /\\ " ++ showTerm(Var x)
showTerm (And term1 term2)     = "(" ++ showTerm(term1) ++ ") /\\ (" ++ showTerm(term2) ++ ")"

-- Expresiones con neg
showTerm (Neg (Var x)) = "neg " ++ showTerm(Var x)
showTerm (Neg term)    = "neg" ++ "(" ++ showTerm(term) ++ ")"

-- Expresiones con ==>
showTerm (Imp (Var x) (Var y)) = showTerm(Var x) ++ " ==> " ++ showTerm(Var y)
showTerm (Imp term (Var y))    = "(" ++ showTerm(term) ++ ") ==> " ++ showTerm(Var y)
showTerm (Imp (Var x) term)    = showTerm(Var x) ++ " ==> (" ++ showTerm(term) ++ ")"
showTerm (Imp term1 term2)     = "(" ++ showTerm(term1) ++ ") ==> (" ++ showTerm(term2) ++ ")"

-- Expresiones con <==>
showTerm (Ioi (Var x) (Var y)) = showTerm(Var x) ++ " <==> " ++ showTerm(Var y)
showTerm (Ioi term (Var y))    = "(" ++ showTerm(term) ++ ") <==> " ++ showTerm(Var y)
showTerm (Ioi (Var x) term)    = showTerm(Var x) ++ " <==> (" ++ showTerm(term) ++ ")"
showTerm (Ioi term1 term2)     = "(" ++ showTerm(term1) ++ ") <==> (" ++ showTerm(term2) ++ ")"

-- Expresiones con !<==>
showTerm (Nioi (Var x) (Var y)) = showTerm(Var x) ++ " !<==> " ++ showTerm(Var y)
showTerm (Nioi term (Var y))    = "(" ++ showTerm(term) ++ ") !<==> " ++ showTerm(Var y)
showTerm (Nioi (Var x) term)    = showTerm(Var x) ++ " !<==> (" ++ showTerm(term) ++ ")"
showTerm (Nioi term1 term2)     = "(" ++ showTerm(term1) ++ ") !<==> (" ++ showTerm(term2) ++ ")"

instance Show Term where show = showTerm

---------------------------------------------------------------
-- Equation
-- Muestra una representacion de las expresiones con === en forma de string

showEquation :: Equation -> String
showEquation (Eq term1 term2) = showTerm(term1) ++ " === " ++ showTerm(term2)

instance Show Equation where show = showEquation

---------------------------------------------------------------
-- Sustitution
-- Muestra una representacion de las expresiones con =: en forma de string

class ShowSust s where
  showSust :: s -> String

instance ShowSust Sust where
  showSust (term1,term2) = "( " ++ showTerm(term1) ++ " =: " ++ showTerm(term2) ++ " )"

instance ShowSust Sust2 where
  showSust (term1, s, term2) = "( " ++ showTerm(term1) ++ "," ++ showSust(s) ++ "," ++ showTerm(term2) ++ " )"

instance ShowSust Sust3 where
  showSust (term1,term2, s, term3,term4) = "( " ++ showTerm(term1) ++ "," ++ showTerm(term2) ++ ","
												  ++ showSust(s) ++ "," ++ showTerm(term3) ++ ","++ showTerm(term4) ++ " )"
