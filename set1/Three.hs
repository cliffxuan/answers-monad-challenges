{- http://mightybyte.github.io/monad-challenges/pages/ex1-3.html -}

import MCPrelude (toLetter, Seed, rand, mkSeed)
import Control.Exception.Base (assert)

type Gen a = Seed -> (a, Seed)

randLetter :: Gen Char
randLetter s =  (toLetter char, newSeed)
    where (char, newSeed) = rand s

generalA :: (Integer -> Integer) -> Gen Integer
generalA f s = (f n, seed)
    where (n, seed) = rand s

randEven :: Gen Integer
randEven = generalA (\n -> 2 * n)

randOdd :: Gen Integer
randOdd = generalA (\n -> 2 * n + 1)

randTen :: Gen Integer
randTen = generalA (\n -> 10 * n)

main :: IO ()
main = print $ assert
    ((product $ map fst $ sequence [randOdd, randEven, randTen] (mkSeed 1)) == 189908109902700)
    "randLetter test passed!"
