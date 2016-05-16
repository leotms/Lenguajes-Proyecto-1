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
