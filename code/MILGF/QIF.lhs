Just some functions based on a tutorial:
"Recent Developments in Quantitative Information Flow" by Geoffrey Smith

> {-# Language RankNTypes #-}
> module QIF where
>
> import Text.PrettyPrint
> import Data.Ratio
> import Control.Monad
> import qualified Data.Map as Map
> import qualified Data.Semigroup as SG
> import Data.Semigroup (Semigroup, Option(..))
> import Data.Maybe(fromJust)
> import Data.Word


Non-empty Lists
===============

"Map reduce" lists

> newtype List a = List { fold :: forall b. (b -> b -> b) -> (a -> b) -> b }
>
> one :: a -> List a
> one a = List (\_ mp -> mp a)
>
> instance Semigroup (List a) where
>   (<>) = append
>
> append :: List a -> List a -> List a
> append (List xs) (List ys) = List (\rd mp -> xs rd mp `rd` ys rd mp)
>
> appends :: List (List a) -> List a
> appends xs = List (\rd mp -> fold xs rd (\l -> fold l rd mp))
>
> instance Functor List where
>   fmap f xs = List (\rd mp -> fold xs rd (mp . f))
>
> instance Applicative List where
>   pure      = one
>   (<*>)     = ap
>
> instance Monad List where
>   xs >>= f  = appends (fmap f xs)
>
> fromFoldable :: Foldable f => f a -> Maybe (List a)
> fromFoldable = getOption . foldMap (Option . Just . one)
>
> fromFoldable' :: Foldable f => f a -> List a
> fromFoldable' = fromJust . fromFoldable



Distributions
=============

> -- | PRE: When using Choice, the list should be a valid distribution
> -- (i.e, probabilities should add up to 1)
> newtype Dist a     = Choice { asList :: List (Rational,a) }

>
> runDist :: Dist a -> (b -> b -> b) -> ((Rational,a) -> b) -> b
> runDist = fold . asList

> instance Functor Dist where
>   fmap = liftM
>
> instance Applicative Dist where
>   pure a = Choice (pure (1,a))
>   (<*>)  = ap
>
> instance Monad Dist where
>   m >>= k = Choice $ do (p1,a) <- asList m
>                         (p2,b) <- asList (k a)
>                         return (p1 * p2, b)
>
> dist :: (Enum a, Bounded a) => [Rational] -> Dist a
> dist xs = Choice $ fromFoldable'
>                  $ (xs ++ repeat 0) `zip` [ minBound .. maxBound ]

> validDist :: Dist a -> Bool
> validDist = (== 1) . (\xs -> fold xs (+) fst) . asList


Channels
========

How likely are we to see the pair (a,b)

> joinMatrix :: Dist a -> (a -> Dist b) -> Dist (a,b)
> joinMatrix pri c = do a <- pri
>                       b <- c a
>                       return (a,b)


> post :: Ord b => Dist a -> (a -> Dist b) -> Dist (Dist a)
> post pri c = Choice $ fmap adj
>                     $ fromFoldable'
>                     $ runDist (joinMatrix pri c) rd mp
>   where
>   mp (p,(x,y)) = Map.singleton y (one (p,x))
>   rd m1 m2     = Map.unionWith append m1 m2
>   adj xs       = let tot = fold xs (+) fst
>                  in (tot, Choice (fmap (\(p,x) -> (p / tot,x)) xs))



Vulnerability
=============

> vulnerability :: Dist a -> Rational
> vulnerability d = runDist d max fst
>
> postVulnerability :: Dist (Dist a) -> Rational
> postVulnerability dd = runDist dd (+) $ \(p,d) -> p * vulnerability d

> minEntropyLeak :: Ord b => Dist a -> (a -> Dist b) -> Double
> minEntropyLeak pri c = logBase 2 (fromRational (v / v'))
>   where v  = vulnerability pri
>         v' = postVulnerability (post pri c)

> gVuln :: List w -> (w -> a -> Rational) -> Dist a -> Rational
> gVuln acts gain pri = fold acts max   $ \w ->
>                       runDist pri (+) $ \(p,x) ->
>                       p * gain w x

> gPostVuln :: List w -> (w -> a -> Rational) -> Dist (Dist a) -> Rational
> gPostVuln acts gain dd = runDist dd (+) $ \(p,d) -> p * gVuln acts gain d

> guessGain :: Eq a => a -> a -> Rational
> guessGain w a = if w == a then 1 else 0

Capacity
========



Pretty Printing
===============

> ppR :: Rational -> Doc
> ppR x
>   | n == 0 = text "0"
>   | n == d = text "1"
>   | otherwise = integer n <> text "/" <> integer (denominator x)
>   where n = numerator x
>         d = denominator x

> ppDist_ :: Dist a -> Doc
> ppDist_ d = runDist d red (ppR . fst)
>   where red d1 d2 = d1 <> comma <+> d2

> ppDist :: (a -> Doc) -> Dist a -> Doc
> ppDist f d = runDist d ($$) mp
>   where mp (p,a) = ppR p <> colon <+> f a

> ppHDist :: Dist (Dist a) -> Doc
> ppHDist = ppDist ppDist_




Examples
========

> data X = X1 | X2 | X3      deriving (Enum,Bounded)
> data Y = Y1 | Y2 | Y3 | Y4 deriving (Enum,Bounded,Eq,Ord)

>
> examplePri :: Dist X
> examplePri  = dist [1/4, 1/2, 1/4]

> exampleChan :: X -> Dist Y
> exampleChan x = case x of
>                   X1 -> dist [1/2,1/2,0,0]
>                   X2 -> dist [0,1/4,1/2,1/4]
>                   X3 -> dist [1/2,1/3,1/6,0]

> example = ppHDist (post examplePri exampleChan)

> biased :: Rational -> Dist a -> Dist a -> Dist a
> biased x d1 d2 = join $ Choice (one (x,d1) `append` one (1-x,d2))

> chan1 :: Word64 -> Dist Word64
> chan1 = \x -> biased (1/8) (return x) (return (-1))
>



