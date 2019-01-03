module ODMatrix.Sim.BusState where


  import Control.Monad.State
  import Data.Functor.Identity (Identity)
  import System.Random
  import System.Random.Shuffle
  import Data.List (sort, group, delete)

  import ODMatrix.Sim.Poisson


  type AnB = (Int, Int) -- Sim result

  -- Sim State
  data BusSt = Bus  {
    passengers :: [Int],
    odlist :: [(Int,Int)],
    currentCell :: Int
  } deriving (Show) 


  -- Initial state
  startBusSt :: BusSt
  startBusSt = Bus [] [] 0


  nextCell :: BusSt -> BusSt
  nextCell b = 
    b {currentCell = (currentCell b) + 1}

  onBoard :: BusSt -> Int
  onBoard = length . passengers
 
  board :: Int -> BusSt -> BusSt
  board n b = b {passengers = passengers b ++ np}
    where np = replicate n $ currentCell b

  -- TODO: El Shuffle es muy arbitrario para la seleccion de pasajeros. I need to be more toughful about that, I will refactor this later.
  alight :: RandomGen g => g -> Int -> BusSt -> BusSt
  alight g n b = b {
    passengers = drop n $ passengers b,
    odlist = odlist b ++ zip (take n $ ps) (replicate n $ currentCell b)
    }
    where ps = shuffle' (passengers b) (length $ passengers b) g
    -- where (sample, nb) = busSampleN n (currentCell b) (mkStdGen 42) (passengers b)


  -- addOD :: (Int,Int) -> Bus -> Bus
  -- addOD od b = b {odlist = od : odlist b}

  busSampleN :: RandomGen a       
             => Int             -- Sample size
             -> Int             -- Current cell
             -> a               -- Random generator
             -> [Int]           -- Current list of passenger
             -> ([Int], [Int])  -- (Sample, new Passenger list)
  busSampleN n c g bus = (rs,bus')
    where (rs,(_,bus')) = runState sq (g, bus)
          sq = (sequence $ replicate n (state $ busSample c))


  busSample _ (_,[]) = error "empty list"
  busSample current (g, xs) = (e, (g2, delete e xs))
    where ps = join $ map (\x -> replicate (current - x) x) xs 
          e = head $ shuffle' ps (length ps) g1
          (g1,g2) = split g
          



  busStep :: Int        -- ^ Capacity
          -> Int        -- ^ Number of cells
          -> PoissonGen -- ^ Generator of boardings
          -> PoissonGen -- ^ Generator of alightings
          -> State BusSt (Int, Int)
  busStep k n bg ag = do

    bus <- get    -- Get the current state from the monadic context
    let current = currentCell bus
    
    -- Make the processing: Get the value and update the state    
    let nb = fst $ random bg
    modify $ board nb
    
    let mx = nb + onBoard bus
        mn = mx - k
        (ag1,ag2) = split ag
        na = if (current + 1) == n 
             then mx 
             else min mx . max mn . fst $ random ag1        
    modify $ alight ag2 na
    
    modify nextCell

    return (nb,na)  -- Wrap the result into the monad



  busSim :: (Num a, RandomGen g) 
         => Int               -- ^ Capacity
         -> [Double]          -- ^ Boarding lambdas
         -> [Double]          -- ^ Alighting lambdas
         -> g                 -- ^ Random generator
         -> [((Int,Int), a)]  -- ^ Sim Result
  busSim k bs as g = 
    map wrapUp . group . sort . odlist $
    execState (sequence $ mkSequence k bs as g) startBusSt
    where wrapUp l = (head l, fromIntegral $ length l)

  
  busSims :: (Num a, RandomGen g) 
          => Int                  -- ^ Capacity
          -> [Double]             -- ^ Boarding lambdas
          -> [Double]             -- ^ Alighting lambdas
          -> g                    -- ^ Random generator
          -> [[((Int,Int), a)]]   -- ^ Sim Result
  busSims k bs as g = map (busSim k bs as) sq
    where sq = iterate (evalState $ state split) g


  mkSequence :: RandomGen g 
             => Int
             -> [Double]
             -> [Double]
             -> g
             -> [State BusSt (Int, Int)]
  mkSequence k bs as g0 = zipWith (busStep k n) bgs ags
    where bgs = mkPoissonGens sb bs
          ags = mkPoissonGens sa as
          n = min (length bs) (length as) -- Number of cells in the route
          (sb,g1) = random g0             -- Random seed for boardings
          sa = fst $ random g1            -- Random seed for alightings


  
  -- Cells randoms

  -- Given a list of lambdas, generates a list of differents Poisson generators with the respective lambdas.
  mkPoissonGens :: Int          -- ^ Seed
                -> [Double]     -- ^ Lambdas
                -> [PoissonGen] -- ^ List of Poisson generators
  mkPoissonGens seed ls = 
    zipWith mkPoissonGen (randoms $ mkStdGen seed) ls
