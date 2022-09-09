import qualified Data.Map as M
-- Set Beats Per minute
setbpm x = setcps (x / 60 / 4)

-- Apply a pattern effect every n bars
fill n = when (\x -> (x `mod` n) == 0)

:{
  -- Infinite series of prime numbers
  -- https://doi.org/10.1017/S0956796808007004
  primes :: [Integer]
  primes = sieve [2 ..]
    where
      sieve xs = sieve' xs M.empty
      sieve' [] table = []
      sieve' (x : xs) table = case M.lookup x table of
        Nothing -> x : sieve' xs (M.insert (x * x) [x] table)
        Just facts -> sieve' xs (foldl reinsert (M.delete x table) facts)
        where
          reinsert table prime = M.insertWith (++) (x + prime) [prime] table
:}

:{
  -- Infinite series of fibonacci numbers
  fibs :: [Integer]
  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
:}
