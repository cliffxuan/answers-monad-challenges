{- http://mightybyte.github.io/monad-challenges/pages/ex1-4.html -}

import MCPrelude (toLetter, Seed, rand, mkSeed)
import Control.Exception.Base (assert)

type Gen a = Seed -> (a, Seed)

randLetter :: Gen Char
randLetter s =  (toLetter int, newSeed)
    where (int, newSeed) = rand s

randPair :: Gen(Char, Integer)
randPair s0 = ((char, int2), s2)
    where (char, s1) = randLetter s0
          (int2, s2) = rand s1

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb s0 = ((a, b), s2)
    where (a, s1) = ga s0
          (b, s2) = gb s1


generalB :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
generalB ga gb f s0 = (f a b, s2)
    where (a, s1) = ga s0
          (b, s2) = gb s1

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 ga gb = generalB ga gb (\a b -> (a, b))


main :: IO ()
main = do
    print $ assert ((fst . randPair $ mkSeed 1) == ('l', 282475249)) "worked 1"
    print $ assert ((fst . (generalPair randLetter rand) $ mkSeed 1) == ('l', 282475249)) "worked 2"
    print $ assert ((fst . (generalPair2 randLetter rand) $ mkSeed 1) == ('l', 282475249)) "worked 3"
