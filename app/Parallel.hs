module Parallel where

import Control.DeepSeq (NFData, force)
import Control.Parallel.Strategies (rpar, rseq, runEval)

-- map a function in parallel, and then applies a reducer by by dichotomy.
-- why is it better than a simple parMap and a single reduce at the end ?
-- because this allows for reduction to happen incrementally, without needing to
-- wait for the map to complete. This saves memory, which is usedful especially
-- if `b` elements are big and numerous.
-- CAVEAT: this does not compute exactly the right thing if the lenght of the list
-- is not a power of 2.
parMapReduceDicho :: (NFData b) => (a -> b) -> (b -> b -> b) -> [a] -> b
parMapReduceDicho _ _ [] = error "cannot reduce empty list"
parMapReduceDicho f _ [x] = f x
parMapReduceDicho f reducer xs =
  let (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs
      results = zipWith process firstHalf secondHalf
   in parMapReduceDicho id reducer results
  where
    -- process :: a -> a -> b
    process a1 a2 = runEval $ do
      b1 <- rpar (force (f a1))
      b2 <- rseq (force (f a2))
      return (reducer b1 b2)