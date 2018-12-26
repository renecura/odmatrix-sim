-- Generate values with a Poisson distribution.
module ODMatrix.Sim.Poisson where

  import System.Random


  data PoissonGen = PoissonGen StdGen Double
    deriving (Show)
 

  mkPoissonGen :: Int         -- ^ Seed
               -> Double      -- ^ Lambda
               -> PoissonGen  -- ^ Poisson Generator
  mkPoissonGen s lambda = PoissonGen (mkStdGen s) lambda


  instance RandomGen PoissonGen where
    next = poissonNext
    split = poissonSplit


  poissonSplit :: PoissonGen -> (PoissonGen,PoissonGen)
  poissonSplit (PoissonGen g l) = (PoissonGen g1 l, PoissonGen g2 l)
    where (g1,g2) = split g


  poissonNext :: PoissonGen -> (Int, PoissonGen)
  poissonNext (PoissonGen g0 lambda) = (k 0 us, PoissonGen g1 lambda)
    where v   = exp (-lambda)
          us  = scanl1 (*) (randoms g0)
          g1  = fst (split g0)
          k n (u:us)
              | u >= v     = k (n+1) us
              | otherwise  = n
          k _ [] = error "PoissonGen.poisson: randoms did not return an infinite list"