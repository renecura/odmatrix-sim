module Main where

  import System.Environment
  import System.Random
  import Numeric.LinearAlgebra (Matrix, assoc, Z, toLists)
  import Control.Monad (join)
  
  import ODMatrix.Sim.BusState



  parseArgs :: [String] -> (Int,Int,Int,Int)
  parseArgs (n:k:s:c:_) = (read n, read k, read s, read c)
  parseArgs _ = error "Insufficients arguments. usage: main route_size capacity seed number_cases"


  main :: IO ()
  main = do
    putStrLn "# ODMatrix Simulation"

    args <- getArgs
    let (n, k, seed, cases) = parseArgs args
        (g1, g2) = split $ mkStdGen seed

    putStrLn "# Generating randoms lambdas..."
    let (bg,ag) = split g1
        bs = take n $ randomRs (0,fromIntegral k) bg
        as = take n $ randomRs (0,fromIntegral k) ag

    putStrLn "# Boarding lambdas"
    mapM_ (putStrLn . show) bs
    putStrLn "# Alighting lambdas"
    mapM_ (putStrLn . show) as

    putStrLn "# Generating origin destination matrices..."
    
    
    let ms = map (assoc (n,n) 0) (busSims k bs as g2) :: [Matrix Z]
    mapM_ (putStrLn . init . tail . show) . take cases . map (join . toLists) $ ms