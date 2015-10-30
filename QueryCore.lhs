Here we provide a Haskell implementation of the Core language from the paper
titled "Dynamic Enforcement of Knowledge-based Security Policies".

> module QueryCore where
>
> import Data.Map as M

It starts of as usual, with the AST:

We've got variables,

> type Var = String

Arithmetic Operators,

> data ArithOp = Add
>              | Mul
>              | Sub
>     deriving (Eq, Show)

And relational operators.

> data RelOp = LEQ
>            | LT
>            | EQ
>            | NEQ
>            | GT
>            | GEQ
>     deriving (Eq, Show)

We combine the above in three ways. Arithmetic expressions,

> data ArithExp = Var Var
>               | Int Int
>               | Aop ArithExp ArithExp ArithExp
>     deriving (Eq, Show)

Boolean expressions,

> data BoolExp = BOp RelOp ArithExp ArithExp 
>              | And BoolExp BoolExp
>              | Or BoolExp BoolExp
>              | Not BoolExp
>     deriving (Eq, Show)

And lastly, Statements.

> data Statement = Skip
>                | Var := ArithExp
>                | IFTE BoolExp Statement Statement
>                | PIFTE Rational Statement Statement
>                | Statement :> Statement
>                | While BoolExp Statement
>     deriving (Eq, Show)

The only non-standard part of this language is the use of _probabilistic if_
statements. These statements take a `Rational` value `q` (between 0 and 1)
instead of a `Bool` and attribute the likelihood of the first branch being
taken at `q` and the second branch at `q - 1`. This way the probability of
either path being taken is `1`. This is actually a generalisation of the
standard if statement, making the standard statement unnecessary semantically.
However, it is still useful, from a reasoning point of view, to have the
standard boolean `if`.

You may have noticed that we can only use `Variables` in `ArithExp`s. Therefore
our state is a mapping from variables to `Int`s.

> type State = Map Var Int
> initialState :: State
> initialState = empty

We can also restrict a given state to a subset of the variables given as a list

> restrictState :: State -> [Var] -> State
> restrictState s vs = filterWithKey (\k _ -> k `elem` vs) s

Because the semantics for this language are probabilistic (as shown by
`PIFTE`), we must consider the nature of _distributions_. Because we are
concerned with the probability of a program being able to reveal secret
information, our distribution is a function from a `State` to a measure of
certainty about the secret information, represented as a Real number in range
[0,1].

> type Dist = Map State Rational

A 'point mass' is a probability distribution that maps a single `State` to 1
and all others to 0.

> point :: State -> Dist
> point s = singleton s 0

There are a few functions over `Dist`s that we require before we can write the
semantics for the language. We can add two distributions together:

> addDist :: Dist -> Dist -> Dist
> addDist = unionWith (+)

We also need to be able to discard irrelevant distributions. For example, when
taking the `True` branch of an `IFTE` statement we can safely disregard the
distributions from the `False` branch. This operation is known as
'conditioning'. We use the function `semB` which defines the semantics for
`BoolExp`s, this allows us to know the value of the `BoolExp` with a given
state of the computation.

> condition :: Dist -> BoolExp -> Dist
> condition d b = mapWithKey (\k v -> if semB b k then v else 0) d

The generalization of conditioning is 'scaling'. This is used for probabilistic
`if`s.

> scale :: Rational -> Dist -> Dist
> scale q = M.map (q *)

The last basic operation on distributions is 'assignment'.  As with `State`s we
will need to be able to reason about a distribution over a restricted subset of
the input state.

A key invariant for `restrictDist` is that the argument to the lambda, `s`,
is already a restricted `State`.

> restrictDist :: Dist -> [Var] -> Dist
> restrictDist d vs = mapKeysWith (+) (flip restrictState vs) d

N.B. It may be necessary to alter this to use the `Dist` type, not clear
yet, it depends on how it's used.

> restrictDist' :: Dist -> [Var] -> State -> Rational
> restrictDist' d vs s = foldrWithKey check 0 d
>   where
>     s' = restrictState s vs -- shouldn't hurt (restrictState idempotent?)
>     check k v x = if restrictState k vs == s' then v + x else x

In a deterministic semantics, assignment would be updating the value of one
variable, like so

> assignState :: Var -> ArithExp -> State -> State
> assignState v exp s = insert v res s
>   where
>     res = semE exp s

Now we can use `assignState` to define the semantics of the assignment operator
for the probabilistic case. In this case the idea is that we update the value
of `v` in _all_ the states in our distribution. The catch is that by doing this
we may cause some states (the keys in our `Dist` Map) to become equal when they
were not beforehand. When this happens we sum their probabilities together.

> assignDist :: Var -> ArithExp -> Dist -> Dist
> assignDist v exp d = mapKeysWith (+) (assignState v exp) d

We can now start describing the overall semantics of the langauge.
Beginning from the 'top': Statements

> semS :: Statement -> Dist -> Dist
> semS Skip          d = d
> semS (v := exp)    d = assignDist v exp d
> semS (IFTE b t f)  d = semS t (condition d b) `addDist`
>                        semS f (condition d (Not b))
> semS (PIFTE q t f) d = semS t (scale q d) `addDist`
>                        semS f (scale (1 - q) d)
> semS (s1 :> s2)    d = semS s2 (semS s1 d)
> semS (While b s)   d = fixDist b s d

We can easily define a fix-point for distributions:

> fixDist :: BoolExp -> Statement -> Dist -> Dist
> fixDist b s d = if d == d'
>                 then d'
>                 else fixDist b s d' `addDist` condition d' (Not b)
>   where
>     d' = semS s (condition d b)

> semB :: BoolExp -> State -> Bool
> semB (Not b)        s = not $ semB b s
> semB (b1 `Or` b2)   s = semB b1 s || semB b2 s
> semB (b1 `And` b2)  s = semB b1 s && semB b2 s
> semB (BOp op e1 e2) s = semRel op (semE e1 s) (semE e2 s)

> semRel :: RelOp -> Int -> Int -> Bool
> semRel op = undefined

> semE :: ArithExp -> State -> Int
> semE b s = undefined
