module Main where
 
  import Data.ConfigFile
  import Data.Either.Utils

  import System.Environment (getArgs)
  import Data.Array ((!), bounds, elems)
  import Numeric.LinearAlgebra (Matrix, Vector, flatten, toList, ident, vector, cols, toLists)
  import System.Random (mkStdGen, randomRs)
    
  import Control.Monad (join)

  import RouteReader
  import ODMatrix.ElementaryDecomposition (applyPath)
  import ODMatrix.ElementaryDecomposition.ShortestPath (sePath)
  -- import Sim.ModSim (modSim)

  import ODMatrix
  import ODMatrix.SmithDecomposition --(computeODM, tVectorSize)

  -- import Visualization.DataTrip ()

  

  import Debug.Trace (trace)
  

  getRoute :: String -> String -> String -> IO Route
  getRoute filename xcol ycol = do 
    content <- readRouteFile filename xcol ycol
    return $ routeSnap (-65.0374881,-42.7675619) 1000 content

  
  parse :: [Double] -> String
  parse = tail . init . show . (map floor)

  

  appendCase :: String 
             -> ODM 
             -> IO ()
  appendCase path odm = do
    let b = inputVector odm
        (p,_) = machine b
        --
        nvedata = parse . toList $ b
        fuldata = parse . join . toLists $ p
        trgdata = parse . toFlatList $ odm
    
    appendFile (path++"_target.csv")       $ trgdata ++ "\n"
    appendFile (path++"_naive_input.csv")  $ nvedata ++ "\n"
    appendFile (path++"_model_input.csv")  $ fuldata ++ "\n"
  

  createTrainingFiles :: String -> [ODM] -> IO ()
  createTrainingFiles path odms = do
    let n = cols . head $ odms
    writeFile (path++"_target.csv")      $ (show n) ++ "\n"
    writeFile (path++"_naive_input.csv") $ (show n) ++ "\n"
    writeFile (path++"_model_input.csv") $ (show n) ++ "\n"    
    mapM_ (appendCase path) odms


  main :: IO ()
  main = do
    
    -- (filename:xcol:ycol:sz:_) <- getArgs
    (config:_) <- getArgs


    val <- readfile emptyCP config
    let cp = forceEither val
        filename = forceEither $ get cp "DEFAULT" "input"
        xcol     = forceEither $ get cp "DEFAULT" "xcol"
        ycol     = forceEither $ get cp "DEFAULT" "ycol"
        sz       = forceEither $ get cp "DEFAULT" "input_size"
        outprefix = forceEither $ get cp "DEFAULT" "output_prefix"
        outsize = forceEither $ get cp "DEFAULT" "output_size"


    -- Retrieve the cells data
    route <- getRoute filename xcol ycol
    
      
    -- 1. Generate a random ODM (target, real) from route
    let --n = length route
        n = read sz :: Int
        (boards, alights) = getRandomInputs 60 n
    
    print n
    
    --odm <- modSim boards alights

    -- odms <- sequence . replicate outsize $ modSim boards alights
    -- createTrainingFiles outprefix odms


    --print "ODM real as Vector (target):"
    --print . (map floor) . toFlatList $ odm
    
    -- 2. Calculate the general solution of the system with the model.
    -- 2.1. Compute the features of the ODM and the vector of independent terms.
    --let b = inputVector odm    

    --print "Vector b (Naive source):"
    --print . (map floor) . toList $ b
    
    -- 2.2. Apply the model to obtain the general solution (implicit).
    -- let gen = computeODM b

    

    -- 3. Apply a random vector t to generate a second random ODM of the same solution set (source, estimation).
    -- TODO: How to generate a random tVector that produces valid odms. Boundaries?
    --print "Vector r1.b (Preprocesed source):"
    --let (p1,r2) = machine b
    --print . (map floor) . join . toLists $ p1



    
    
    
    
    
    
    -- let vt = vector $ 20.0:(replicate (tVectorSize n - 1) 0.0)
    --     odm' = gen vt
    
    -- print "Vector t:"
    -- print vt

    -- print "ODM Estimated:"
    -- print odm'

    
    -- 4. Compute the shortest path from source to target.
    -- 4.1. (Optional, the model must assure this) Verify if src and trg are in the same solution set.
    -- 4.2 Compute the shortest path.
   -- let path = sePath (cellMeasure route) odm' odm
    --    rs = applyPath odm' path
    
    -- 5. Calculate de value of the path.


    -- 6. Verification of the results.
   -- print path
    --print rs
   -- print $ rs == odm
    putStrLn "Ready!"
    return ()

  
  

  
  -- | Compute the difference of two cells in the route.
  cellMeasure :: Route -> Int -> Int -> Double
  cellMeasure route cellA cellB = 
    sqrt $ (x0-x1)**2 + (y0-y1)**2
    where (x0,y0) = route!(cellA+1)
          (x1,y1) = route!(cellB+1)
  

  
  
  -- TODO: Better simulation.
  getRandomInputs :: Int            -- ^ Capacity
                  -> Int            -- ^ Size
                  -> ([Int], [Int]) -- ^ (boards, alights)
  getRandomInputs k n =
    let gen = mkStdGen 1
        boards  = take n $ randomRs (0,k) gen
        alights = take n $ randomRs (0,k) gen
    in (boards, alights)


  
  getRandomMatrix :: ([Int], [Int])     -- ^ (boards, alights)
                  -> IO (Matrix Double) -- ^ Random od-matrix
  getRandomMatrix (boards, alights) = modSim boards alights