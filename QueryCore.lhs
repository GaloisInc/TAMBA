Here we provide a Haskell implementation of the Core language from the paper
titled "Dynamic Enforcement of Knowledge-based Security Policies".

> module QueryCore where
>
> import Data.Map

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

Because the semantics for this language are probabilistic (as shown by PIFTE),
we must consider the nature of _distributions_. Because we are concerned with
the probability of a program being able to reveal secret information, our
distribution is a function from a `State` to a measure of certainty about the
secret information, represented as a Real number in range [0,1].

> type Dist = State -> Rational

A 'point mass' is a probability distribution that maps a single `State` to 1
and all others to 0.

> point :: State -> Dist
> point s = \s' -> if s == s
>                  then 1
>                  else 0

There are a few functions over `Dist`s that we require before we can write the
semantics for the language. We can add two distributions together:

> addDist :: Dist -> Dist -> Dist
> addDist d1 d2 = (+) <$> d1 <*> d2

We also need to be able to discard irrelevant distributions. For example, when
taking the `True` branch of an `IFTE` statement we can safely disregard the
distributions from the `False` branch. This operation is known as
'conditioning'. We use the function `semB` which defines the semantics for
`BoolExp`s, this allows us to know the value of the `BoolExp` in the current
state of the computation.

> condition :: Dist -> BoolExp -> Dist
> condition d b = \s -> if semB b s
>                       then d s 
>                       else 0

The generalization of conditioning is 'scaling'. This is used for probabilistic
`if`s.

> scale :: Rational -> Dist -> Dist
> scale q d = \s -> q * (d s)

The last basic operation on distributions is 'assignment'. 
As with `State`s we will need to be able to reason about a distribution over a
restricted subset of the input state.

> --restrictDist :: Dist -> [Var] -> Dist
> --restrictDist d vs = \s -> 

> semB :: BoolExp -> State -> Bool
> semB b s = undefined
