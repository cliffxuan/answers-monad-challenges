{- http://mightybyte.github.io/monad-challenges/pages/ex1-5.html -}

import MCPrelude (toLetter, Seed, rand, mkSeed)
import Control.Exception.Base (assert)

randLetter :: Seed -> (Char, Seed)
randLetter seed = (toLetter char, newSeed)
    where (char, newSeed) = rand seed

type Gen a = Seed -> (a, Seed)

repRandom :: [Gen a] -> Gen [a]
repRandom [] initialSeed = ([], initialSeed)
repRandom (x:xs) initialSeed = (r:rs, finalSeed)
    where (r, firstSeed) = x initialSeed
          (rs, finalSeed) = repRandom xs firstSeed

main :: IO ()
main = print $ assert
    ((fst $ repRandom (replicate 3 randLetter) (mkSeed 1)) == "lrf")
    "passed!"
