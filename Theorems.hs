module Theorems where

import Terms

prop :: Float -> Equation
prop num
  | num == 3.1  = (p <==> q) <==> r === p <==> (q <==> r)     -- axiom
  | num == 3.2  = (p <==> q) === (q <==> p)                   -- axiom
  | num == 3.3  = true === p <==> p                           -- axiom
  | num == 3.4  = p === p <==> true                           -- axiom
  | num == 3.5  = (p <==> q) <==> q === p                     -- theorem
  | num == 3.6  = p === (q <==> q) <==> p                     -- theorem
  | num == 3.7  = (p <==> p) <==> (q <==> q) === true         -- theorem
  | num == 3.8  = false === neg true                          -- axiom
  | num == 3.9  = neg (p <==> q) === neg p <==> q             -- axiom
  | num == 3.101 = p !<==> q === neg (p <==> q)               -- axiom
  | num == 3.11 = neg p === q <==> (p <==> neg q)             -- theorem
  | num == 3.12 = neg (neg p) === p                           -- theorem
  | num == 3.13 = neg false === true                          -- theorem
  | num == 3.14 = p !<==> q === neg p <==> q                  -- theorem
  | num == 3.15 = neg p <==> p === false                      -- theorem
  | num == 3.16 = p !<==> q === q !<==> p                     -- theorem
  | num == 3.17 = (p !<==> q) !<==> r === p !<==> (q !<==> r) -- theorem
  | num == 3.18 = (p !<==> q) <==> r === p !<==> (q <==> r)   -- theorem
  | num == 3.19 = (p !<==> q) <==> r === p <==> (q !<==> r)   -- theorem
  | otherwise = error "The statement doesn't exists"