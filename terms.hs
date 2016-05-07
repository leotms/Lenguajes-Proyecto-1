-- Definicion de los Terminos Term
-- Autores:
-- Aldrix Marfil     10-10940
-- Leonardo Martinez 11-10576

-- Definicion de Term
data Term = Var Char
      | Or   Term Term
      | Neg  Term
      | And  Term Term
      | Imp  Term Term
      | Ioi  Term Term
      | Nioi Term Term
      deriving Show

-- Operadores

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

-- Terminos para el abecedario
a :: Term
a  = Var 'a'

b :: Term
b  = Var 'b'

c :: Term
c = Var 'c'

d :: Term
d = Var 'd'

e :: Term
e  = Var 'e'

f :: Term
f  = Var 'f'

g :: Term
g = Var 'g'

h :: Term
h = Var 'h'

i :: Term
i  = Var 'i'

j :: Term
j  = Var 'j'

k :: Term
k = Var 'k'

l :: Term
l = Var 'l'

m :: Term
m  = Var 'm'

n :: Term
n  = Var 'n'

o :: Term
o = Var 'o'

p :: Term
p = Var 'p'

q :: Term
q  = Var 'q'

r :: Term
r  = Var 'r'

s :: Term
s = Var 's'

t :: Term
t = Var 't'

u :: Term
u  = Var 'u'

v :: Term
v  = Var 'v'

w :: Term
w = Var 'w'

x :: Term
x = Var 'x'

y :: Term
y = Var 'y'

z :: Term
z = Var 'z'
