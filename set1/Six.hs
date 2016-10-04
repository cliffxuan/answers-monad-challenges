{- http://mightybyte.github.io/monad-challenges/pages/ex1-6.html -}

import MCPrelude (toLetter, Seed, rand, mkSeed)
import Control.Exception.Base (assert)

type Gen a = Seed -> (a, Seed)

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g f s0 = (f x) s1
    where (x, s1) = g s0

mkGen :: a -> Gen a
mkGen x s = (x, s)
