{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Definicion de las reglas para la deduccion logica
-- Autores:
-- Aldrix Marfil     10-10940
-- Leonardo Martinez 11-10576

-- Modulos necesarios
import Terms

-- Funciones para la Sustitucion

class Sustitution s where
	sust :: Term -> s -> Term

instance Sustitution Sust where
	-- caso base
	sust (Var t) (x,p)   = if (Var t) == p then x else (Var t)
	sust (Or t1 t2) s    = Or (sust t1 s) (sust t2 s)
	sust (And t1 t2) s   = And (sust t1 s) (sust t2 s)
	sust (Neg t1) s      = Neg (sust t1 s)
	sust (Imp t1 t2) s   = Imp (sust t1 s) (sust t2 s)
	sust (Ioi t1 t2) s   = Ioi (sust t1 s) (sust t2 s)
	sust (Nioi t1 t2) s  = Nioi (sust t1 s) (sust t2 s)

instance Sustitution (Term, Sust, Term) where
	-- caso base
	sust (Var t) (x, (y,p), q) = if (Var t) ==  p then y else if (Var t) == q then x else (Var t)
	sust (Or t1 t2) s    = Or (sust t1 s) (sust t2 s)
	sust (And t1 t2) s   = And (sust t1 s) (sust t2 s)
	sust (Neg t1) s      = Neg (sust t1 s)
	sust (Imp t1 t2) s   = Imp (sust t1 s) (sust t2 s)
	sust (Ioi t1 t2) s   = Ioi (sust t1 s) (sust t2 s)
	sust (Nioi t1 t2) s  = Nioi (sust t1 s) (sust t2 s)

instance Sustitution (Term, Term, Sust, Term, Term) where
	-- caso base
	sust (Var t) (x, y, (z,p), q, r) = if (Var t) == p then z else if (Var t) == q then y else if (Var t) == r then x else (Var t)
	sust (Or t1 t2) s    = Or (sust t1 s) (sust t2 s)
	sust (And t1 t2) s   = And (sust t1 s) (sust t2 s)
	sust (Neg t1) s      = Neg (sust t1 s)
	sust (Imp t1 t2) s   = Imp (sust t1 s) (sust t2 s)
	sust (Ioi t1 t2) s   = Ioi (sust t1 s) (sust t2 s)
	sust (Nioi t1 t2) s  = Nioi (sust t1 s) (sust t2 s)

---------------------------------------------------------------
-- Instanciacion

class Instantiate s where
	instantiate :: Equation -> s -> Equation

instance Instantiate Sust where
	instantiate (Eq t1 t2) s = Eq (sust t1 s) (sust t2 s)

instance Instantiate (Term, Sust, Term) where
	instantiate (Eq t1 t2) s = Eq (sust t1 s) (sust t2 s)

instance Instantiate (Term, Term, Sust, Term, Term) where
	instantiate (Eq t1 t2) s = Eq (sust t1 s) (sust t2 s)

---------------------------------------------------------------
-- Regla de Leibniz

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Eq t1 t2) e (Var z) = Eq (sust e (t1 =: (Var z))) (sust e (t2 =: (Var z)))

---------------------------------------------------------------
-- Inferencia

-- Clase polimorfica de Inferencia, para los diferentes sust.
class Inference s where
	infer :: (Num n) => n -> Equation -> s -> Term -> Term -> Equation

instance Inference Sust where
	infer n eq s (Var z) e = leibniz (instantiate eq s) e (Var z)

instance Inference (Term, Sust, Term) where
	infer n eq s (Var z) e = leibniz (instantiate eq s) e (Var z)

instance Inference (Term, Term, Sust, Term, Term) where
	infer n eq s (Var z) e = leibniz (instantiate eq s) e (Var z)

---------------------------------------------------------------
-- Deduccion de un paso

-- Funciones auxiliares
eq_izq :: Equation -> Term
eq_izq (Eq t1 t2) = t1

eq_der :: Equation -> Term
eq_der (Eq t1 t2) = t2

step :: (Num n) => Term -> n-> Equation -> Sust -> Term -> Term -> Term
step t n eq s (Var z) e
	| eq_izq x == t  = eq_der x
	| eq_der x == t  = eq_izq x
	| otherwise = error "Error\n"
	where x = infer n eq s (Var z) e

---------------------------------------------------------------
	-- Funciones Dummy

-- recibe un z y retorna el z
lambda :: (z) -> z
lambda z = z

-- recibe un lambda y retorna lambda
using :: (l) -> l
using l = l

-- recibe una Sust y retorna una Sust
with :: (s) -> s
with s = s

-- statement :: Term -> IO (Term)
-- statement t
