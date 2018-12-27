-- {-# DeriveDataTypeable #-}
module Main where

  import System.Environment
  import System.Random
  import Numeric.LinearAlgebra (Matrix, assoc, Z, toLists)
  import Control.Monad (join)

  import System.Console.CmdArgs.Implicit
  
  import ODMatrix.Sim.BusState



  data SimParams = Params {
      size :: Int,
      capacity :: Int,
      seed :: Int,
      cases :: Int,
      lambdas :: Bool
  } deriving (Show, Data, Typeable)

  params = Params {
    size = 10 &= help "Size of the route",
    capacity = 50 &= help "Capacity of the unit",
    seed = 0 &= help "Seed for the simulation",
    cases = 10 &= help "Number of cases to simulate",
    lambdas = False &= help "Indicates if the lists of lambdas gone to be provided manually"
  }


  



  main :: IO ()
  main = do
    putStrLn "# ODMatrix Simulation"

    args <- cmdArgs params
    print args

    let (Params n k seed cases manual_lambdas) = args
        (g1, g2) = split $ mkStdGen seed

    putStrLn $  if manual_lambdas 
                then "# Reading lambdas..."
                else "# Generating randoms lambdas..."

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