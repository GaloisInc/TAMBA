module StandardSemantics where

import QueryCore
import Data.Map as M

sem :: Statement -> State -> State
sem Skip          s = s
sem (v := exp)    s = insert v (semE exp s) s
sem (IFTE p t f)  s = case semB p s of
                            True  -> sem t s
                            False -> sem f s
sem (s1 :> s2)    s = sem s2 (sem s1 s)
sem (While p e)   s = if p'
                      then sem (While p e) (sem e s)
                      else s
  where
    p' = semB p s
sem (PIFTE q a b) s = error "No probabilistic `if` in the simple semantics"
