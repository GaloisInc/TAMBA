> module Matricize where

> import Prelude hiding (lookup)
> import Data.List hiding (lookup)
> import Data.Map
> import Data.Maybe (fromJust)
> import QueryCore
> import StandardSemantics
> import Data.List.Matrix

```
--------------------------------------------------------------------------------
-------------------- Warning: Hacky, non-general code below --------------------
--------------------------------------------------------------------------------
```

The code below is how we convert the queries into matrices. Right now we are
not concerning ourselves with being as general as possible. This means that
we've taken shortcuts in our implementation (to make it simpler) by taking
advantage of properties of the specific queries we are looking at initially.

I can already tell you what needs to be generalized for this to work for all
queries:

* Don't assume that the inputs and outputs are integers (or that the output
  values represent Bools)
* Don't assume that the output set is continuous
* Don't assume that the queries are deterministic (i.e. that each row of the
  resulting matrix is a unitVector)
* Don't use lists of lists for everything. It will quickly become intractable


```
--------------------------------------------------------------------------------
-------------------- End of caveats about the code below -----------------------
--------------------------------------------------------------------------------
```


We can enumerate all possible inputs (as long as they're bounded) pretty
easily.

> runAllInputs :: Statement -> (Int, Int) -> [State]
> runAllInputs s (min, max) = fmap (sem s) $ fmap (singleton "i") [min..max]

Given the list of all resulting states, we can just as easily get the list of
all the outputs. Because of how we created the list of inputs above, we know
that the indices of the resulting list represent the indices of the input
Vector, i.e. the ith element of the resulting list is the output from the ith
input.

> allOutputs :: [State] -> [Int]
> allOutputs ss = fmap getOutput ss
>   where
>     getOutput s = case lookup "o" s of
>                         Just v  -> v
>                         Nothing -> error ("No output in state " ++ show s)

It's also trivial to get the unique outputs from all the states.

> outputSet :: [State] -> [Int]
> outputSet = sort . nub . allOutputs

Let's now use the above to make a function that takes a query, a range of input values
and returns the channel matrix.

> matricize :: Statement -> (Int, Int) -> Matrix Int
> matricize q bnds = fmap indecize outs
>   where
>     outs = allOutputs $ runAllInputs q bnds
>     uOuts = sort $ nub $ outs -- Unique outputs
>     nOuts = length uOuts      -- number of different outputs
>     indecize i = unitVector nOuts $ fromJust $ elemIndex i uOuts -- Note: [1]

Note #1: The fromJust is safe here because we know that any element in outs
must also be in its 'nub'ed version uOuts.

We can perform a similar process (and save on some redundant computation) in order
to convert a query into a full channel represenation

> chanelize :: Num a => Statement -> (Int, Int) -> ([Int], [Int], [[a]])
> chanelize q b@(min, max) = (ins, uOuts, mat)
>   where
>     ins        = [min..max]
>     outs       = allOutputs $ runAllInputs q b
>     mat        = fmap indecize outs
>     uOuts      = sort $ nub $ outs -- Unique outputs
>     nOuts      = length uOuts      -- number of different outputs
>     indecize i = unitVector nOuts $ fromJust $ elemIndex i uOuts -- Note: [1]
