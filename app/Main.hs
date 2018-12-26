module Main where

  import System.Random
  import Numeric.LinearAlgebra (Matrix, assoc, Z)
  
  import ODMatrix.Sim.BusState

  main :: IO ()
  main = do
    print "ODMatrix Simulation"
    let n = 20

    print "Generating randoms lambdas..."
    let (bg,ag) = split $ mkStdGen 0
        bs = take n $ randomRs (0,50) bg
        as = take n $ randomRs (0,50) ag

    print bs
    print as

    print "Generating origin destination matrices..."
    
    let m = assoc (n,n) 0 (busSim 60 bs as 4) :: Matrix Z
    print m