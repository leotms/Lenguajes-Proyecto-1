{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Definicion de las reglas para la deduccion logica y verificacion de teoremas.
-- Autores:
-- Aldrix Marfil     10-10940
-- Leonardo Martinez 11-10576

-- Para que sea un modulo importable
module Rules where

-- Modulos necesarios:
-- Terms:    Contiene las definiciones de terminos, tipos y sus impresiones.
-- Theorems: Contiene las definiciones de los teoremas y axiomas necesarios
--           para las demostraciones.

import Terms
import Theorems

-- Funciones para la Sustitucion

class Sustitution s where
	sust :: Term -> s -> Term

instance Sustitution Sust where
	-- caso base
	sust (Var t) (x,p)   = if (Var t) == p then x else (Var t)
	sust (Bool b)   s    = Bool b
	sust (Or t1 t2) s    = Or (sust t1 s) (sust t2 s)
	sust (And t1 t2) s   = And (sust t1 s) (sust t2 s)
	sust (Neg t1) s      = Neg (sust t1 s)
	sust (Imp t1 t2) s   = Imp (sust t1 s) (sust t2 s)
	sust (Ioi t1 t2) s   = Ioi (sust t1 s) (sust t2 s)
	sust (Nioi t1 t2) s  = Nioi (sust t1 s) (sust t2 s)

instance Sustitution Sust2 where
	-- caso base
	sust (Var t) (x, (y,p), q) = if (Var t) ==  p then x else if (Var t) == q then y else (Var t)
	sust (Bool b)   s    = Bool b
	sust (Or t1 t2) s    = Or (sust t1 s) (sust t2 s)
	sust (And t1 t2) s   = And (sust t1 s) (sust t2 s)
	sust (Neg t1) s      = Neg (sust t1 s)
	sust (Imp t1 t2) s   = Imp (sust t1 s) (sust t2 s)
	sust (Ioi t1 t2) s   = Ioi (sust t1 s) (sust t2 s)
	sust (Nioi t1 t2) s  = Nioi (sust t1 s) (sust t2 s)

instance Sustitution Sust3 where
	-- caso base
	sust (Var t) (x, y, (z,p), q, r) = if (Var t) == p then x else if (Var t) == q then y else if (Var t) == r then z else (Var t)
	sust (Bool b)   s    = Bool b
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

instance Instantiate Sust2 where
	instantiate (Eq t1 t2) s = Eq (sust t1 s) (sust t2 s)

instance Instantiate Sust3 where
	instantiate (Eq t1 t2) s = Eq (sust t1 s) (sust t2 s)

---------------------------------------------------------------
-- Regla de Leibniz

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Eq t1 t2) e (Var z) = Eq (sust e (t1 =: (Var z))) (sust e (t2 =: (Var z)))

---------------------------------------------------------------
-- Inferencia

-- Clase polimorfica de Inferencia, para los diferentes sust.
class Inference s where
	infer :: Float -> s -> Term -> Term -> Equation

instance Inference Sust where
	infer n s (Var z) e = leibniz (instantiate (prop n) s) e (Var z)

instance Inference Sust2 where
	infer n s (Var z) e = leibniz (instantiate (prop n) s) e (Var z)

instance Inference Sust3 where
	infer n s (Var z) e = leibniz (instantiate (prop n) s) e (Var z)

---------------------------------------------------------------
-- Deduccion de un paso

-- Funciones auxiliares
eq_izq :: Equation -> Term
eq_izq (Eq t1 t2) = t1

eq_der :: Equation -> Term
eq_der (Eq t1 t2) = t2

class Step s where
	step :: Term -> Float -> s -> Term -> Term -> Term

instance Step Sust where
	step t n s (Var z) e
		| eq_izq x == t  = eq_der x
		| eq_der x == t  = eq_izq x
		| otherwise = error "Invalid inference rule.\n"
		where x = infer n s (Var z) e

instance Step Sust2 where
	step t n s (Var z) e
		| eq_izq x == t  = eq_der x
		| eq_der x == t  = eq_izq x
		| otherwise = error "Invalid inference rule.\n"
		where x = infer n s (Var z) e

instance Step Sust3 where
	step t n s (Var z) e
		| eq_izq x == t  = eq_der x
		| eq_der x == t  = eq_izq x
		| otherwise = error "Invalid inference rule.\n"
		where x = infer n s (Var z) e

---------------------------------------------------------------
-- Funciones Dummy

-- recibe un z y retorna el z
lambda :: String
lambda  = "lambda"

-- recibe un lambda y retorna lambda
using :: String
using  = "using"

-- recibe una Sust y retorna una Sust
with :: String
with  = "with"

class Statement s where
	statement :: Float -> String -> s -> String -> String -> Term -> Term -> Term -> IO Term

instance Statement Sust where
	statement n w s u l t1 t2 t0 = do { putStrLn ("=== <statement " ++ show(n) ++ " " ++ w ++ " " ++ showSust(s) ++ " " ++ l ++ " " ++ showTerm(t1) ++ " (" ++ showTerm(t2) ++ ")>") ;
																			putStrLn (showTerm(step t0 n s t1 t2));
																			return (step t0 n s t1 t2)}
instance Statement Sust2 where
	statement n w s u l t1 t2 t0 = do { putStrLn ("=== <statement " ++ show(n) ++ " " ++ w ++ " " ++ showSust(s) ++ " " ++ l ++ " " ++ showTerm(t1) ++ " (" ++ showTerm(t2) ++ ")>") ;
																			putStrLn (showTerm(step t0 n s t1 t2));
																			return (step t0 n s t1 t2)}

instance Statement Sust3 where
	statement n w s u l t1 t2 t0 = do { putStrLn ("=== <statement " ++ show(n) ++ " " ++ w ++ " " ++ showSust(s) ++ " " ++ l ++ " " ++ showTerm(t1) ++ " (" ++ showTerm(t2) ++ ")>") ;
																			putStrLn (showTerm(step t0 n s t1 t2));
																			return (step t0 n s t1 t2)}

proof :: Equation -> IO Term
proof eq = do {putStrLn ("prooving " ++ show(eq) ++ "\n");
               putStrLn (showTerm(eq_izq eq));
							 return (eq_izq eq)}

done ::  Equation -> Term -> IO ()
done eq end = if eq_der eq == end then putStrLn "\n proof successful" else putStrLn "\n proof unsuccessful"
