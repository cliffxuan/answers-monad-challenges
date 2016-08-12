{- http://mightybyte.github.io/monad-challenges/pages/ex1-1.html -}

import MCPrelude (rand, mkSeed, Seed)
import Control.Exception.Base (assert)

nRands :: Integer -> [(Integer, Seed)]
nRands 1 = [rand $ mkSeed 1]
nRands n = (rand . snd . head $ prev) : prev
    where prev = nRands $ n - 1

fiveRands :: [Integer]
fiveRands = map fst $ nRands 5

main :: IO ()
main = print $ assert
    (product fiveRands == 8681089573064486461641871805074254223660)
    "fiveRands test passed!"
