module RMprime (
    prime,
    prime1,
    ndprime,
    ndprime1
) where

import Control.Monad.Random
import Ebs


-- rs n = (r,s) such that s is odd and 2^r * s = n.
rs :: Integer -> (Integer,Integer)
rs n = rsaux (0,n)
       where rsaux (r,s) | mod s 2 == 0 = rsaux (r + 1,div s 2)
                         | otherwise    = (r,s)


-- testrest n ($,...,$,n-1,1,%,...,%) = true
--        where $ is not 1 or n-1
--          and % are arbitrary numbers
-- testrest n other = false
testrest :: Integer -> [Integer] -> Bool
testrest n []       = False
testrest n [x]      = False
testrest n (x:y:xs) = (x==(n-1)) && (y==1) || testrest n (y:xs)


-- testseq n (1,%,...,%) = true
-- testseq n ($,...,$,n-1,1,%,...,%) = true
--    where $ is not 1 or n-1
--      and % are arbitrary numbers
-- testseq n other = false
testseq :: Integer -> [Integer] -> Bool
testseq n []     = False
testseq n (1:xs) = True
testseq n xs     = testrest n xs


-- mkseqaux n b s r creates the list
--     [ b^(n-1) mod n, ... , b^(2s) mod n, b^s mod n ]
--     with 2^r * s = n.
mkseqaux :: Integer -> Integer -> Integer -> Integer -> [Integer]
mkseqaux n b s (-1) = []
mkseqaux n b s r  = ebsmod b ((ebs 2 r) * s) n : mkseqaux n b s (r-1)


-- mkseq n b creates a list
--     [ b^s mod n, b^(2s) mod n, ... , b^(n-1) mod n]
--     with 2^r * s = n.
mkseq :: Integer -> Integer -> [Integer]
mkseq n b = reverse (mkseqaux n b s r)
            where (r,s) = rs (n - 1)


-- This is a single Rabin-Miller primality test of n with a given b
-- such that 1 < b < n.
-- if prime n b = true
--    then n is prime or n is not prime
--         with a probability of < 1/2
--    else n is not prime.
prime1 :: Integer -> Integer -> Bool
prime1 n b = if (gcd n b) == 1
                then testseq n (mkseq n b)
                else False


-- ndprime1 n r tests the number n with a random number provided by r.
ndprime1 :: (MonadRandom m) => Integer -> m Integer -> m Bool
ndprime1 n = fmap (prime1 n)


allTrue :: [Bool] -> Bool
allTrue = foldl (&&) True


-- ndprime tests n with k random bs.
--    r is a Random.rand structure.
ndprime :: (MonadRandom m) => Int -> Integer -> m Bool
ndprime k n = fmap allTrue $ sequence $ map (ndprime1 n) $ replicate k r
    where r = getRandomR (2,n-1)


-- prime tests n with 20 random bs.
prime :: (MonadRandom m) => Integer -> m Bool
prime n = ndprime 20 n
