module ODMatrix.Sim.BusState where


  import Control.Monad.State
  import System.Random
  import System.Random.Shuffle
  import Data.List (sort, group)

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

  -- freeSpace :: Bus -> Int
  -- freeSpace b = (capacity b) - (onBoard b)


  -- -- samplePassengers :: Bus -> Int -> [Int]
  -- -- samplePassengers b n = []
  
  board :: Int -> BusSt -> BusSt
  board n b = b {passengers = passengers b ++ np}
    where np = replicate n $ currentCell b

  -- TODO: El Shuffle es muy arbitrario para la seleccion de pasajeros. I need to be more toughful about that, I will refactor this later.
  alight :: Int -> BusSt -> BusSt
  alight n b = b {
    passengers = drop n $ passengers b,
    odlist = odlist b ++ zip (take n $ ps) (replicate n $ currentCell b)
    }
    where ps = shuffle' (passengers b) (length $ passengers b) (mkStdGen 42)


  -- addOD :: (Int,Int) -> Bus -> Bus
  -- addOD od b = b {odlist = od : odlist b}



  busStep :: Int        -- ^ Capacity
          -> PoissonGen -- ^ Generator of boardings
          -> PoissonGen -- ^ Generator of alightings
          -> State BusSt (Int, Int)
  busStep k bg ag = do

    bus <- get    -- Get the current state from the monadic context
    
    -- Make the processing: Get the value and update the state
    
    let nb = fst $ random bg
    modify $ board nb
    
    let mx = nb + onBoard bus
        mn = mx - k
        na = min mx . max mn . fst $ random ag
    modify $ alight na
    
    modify nextCell

    return (nb,na)  -- Wrap the result into the monad



  busSim :: Num a 
         => Int         -- ^ Capacity
         -> [Double]    -- ^ Boarding lambdas
         -> [Double]    -- ^ Alighting lambdas
         -> Int         -- ^ Seed
         -> [((Int,Int), a)] -- ^ Sim Result
  busSim k bs as s = 
    map (\l -> (head l, fromIntegral $ length l)) . group . sort . odlist $ snd fs
    where fs = runState (sequence $ zipWith (busStep k) bgs ags) startBusSt
          [sb,sa] = take 2 $ randoms (mkStdGen s)
          bgs = mkPoissonGens sb bs
          ags = mkPoissonGens sa as

  


  
  -- Cells randoms

  -- Given a list of lambdas, generates a list of differents Poisson generatos with the respective lambdas.
  mkPoissonGens :: Int          -- ^ Seed
                -> [Double]     -- ^ Lambdas
                -> [PoissonGen] -- ^ List of poisson generators
  mkPoissonGens seed ls = 
    zipWith mkPoissonGen (randoms $ mkStdGen seed) ls
