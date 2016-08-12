{- http://mightybyte.github.io/monad-challenges/pages/ex1-2.html -}

import MCPrelude (toLetter, Seed, rand, mkSeed)
import Control.Exception.Base (assert)

randLetter :: Seed -> (Char, Seed)
randLetter seed = (toLetter char, newSeed)
    where (char, newSeed) = rand seed

randStringN :: Integer -> [(Char, Seed)]
randStringN 1 = [randLetter . mkSeed $ 1]
randStringN n = (randLetter . snd . head $ prev) : prev
    where prev = randStringN $ n - 1

randString3 :: String
randString3 = reverse $ map fst $ randStringN 3

main :: IO ()
main = print $ assert
    (randString3 == "lrf")
    "randString3 test passed!"
