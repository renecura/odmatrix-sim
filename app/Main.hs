-- {-# DeriveDataTypeable #-}
module Main where

  import System.Environment
  import System.Random
  import Numeric.LinearAlgebra (Matrix, assoc, Z, toLists)
  import Control.Monad (join)
  import Data.Char (toLower)

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
    lambdas = False &= help "Indicates if the lists of lambdas will be provided manually"
  }


  getLambdas :: RandomGen g => Bool -> g -> Int -> Int -> IO ([Double],[Double])
  getLambdas False g n k = do -- Random genetated
    let (bg,ag) = split g
        bs = take n $ randomRs (0,fromIntegral k) bg
        as = take n $ randomRs (0,fromIntegral k) ag
    return (bs, as) 
  getLambdas True _ n _ = do  -- Manually provided
    input <- getContents
    let ls = lines input
        bs = take n ls
        as = take n $ drop n ls
    return (map read bs, map read as)




  main :: IO ()
  main = do
    -- ODMatrix Simulation
    putStrLn "{"

    args <- cmdArgs params
    
    let (Params n k seed cases manual_lambdas) = args
        (g1, g2) = split $ mkStdGen seed

    putStrLn "\"params\": {"
    putStrLn $ "\"size\":" ++ (show n) ++ "," 
    putStrLn $ "\"capacity\":" ++ (show k) ++ "," 
    putStrLn $ "\"seed\":" ++ (show seed) ++ "," 
    putStrLn $ "\"cases\":" ++ (show cases) ++ "," 
    putStrLn $ "\"manual_lambdas\":" ++ (map toLower $ show manual_lambdas)
    putStrLn "},"

    -- putStrLn $  if manual_lambdas 
    --             then "# Reading lambdas..."
    --             else "# Generating randoms lambdas..."

    (bs,as) <- getLambdas manual_lambdas g1 n k

    putStrLn "\"lambdas\" : {"
    
    -- Boarding lambdas
    putStr "\"boards\": "    
    putStr . show $ bs
    putStrLn ","
    
    -- Alighting lambdas
    putStr "\"alights\": "    
    putStrLn . show $ as

    putStrLn "},"


    -- Generating origin destination matrices
    
    putStrLn "\"odms\": {"
    let ms = map (assoc (n,n) 0) (busSims k bs as g2) :: [Matrix Z]
        ms' = map (\(i,m) -> "\"" ++ (show i) ++ "\": " ++ (show m) ++ ",\n") .
              zip [0..] .
              take cases . map (join . toLists) $ ms


    putStrLn . init . init . join $ ms'
    putStrLn "}"
    
    putStrLn "}"