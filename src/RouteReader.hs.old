module RouteReader (
    readRouteFile,
    cellSnap,
    routeSnap,
    Route, 
    Array
  ) where

  import Data.Array
  import CSVMapParser

  -- | Define the route as a sequence of cells
  type Route = Array Int (Double, Double)

  -- | Equivalent to Route but with a different semantic.
  type Points = Array Int (Double, Double)
  
  
  -- | Read an array of points that describes a route from a csv file.
  -- This function assumes that the data in the file is ordered in sequence.
  readRouteFile :: String -- ^ Filename
                -> String -- ^ latitude column
                -> String -- ^ longitud column
                -> IO Points  -- ^ Array of points
  readRouteFile filename lat lon = do    
    dataMap <- readCSV ',' filename
    let dat = unpackData dataMap 
        coords = zip (dat lat) (dat lon)
    return $ listArray (1,length coords) coords
  
  
  -- | Snap the data to a square grid.
  cellSnap :: (Double, Double) -- ^ Origin
           -> Double           -- ^ Scale
           -> (Double, Double) -- ^ Point
           -> (Double, Double) -- ^ Snapped point
  cellSnap (x0, y0) scl (x, y) =
    ( fromInteger . round $ scl * (x - x0),
      fromInteger . round $ scl * (y - y0) )
  
  
  -- | Snap each point in sequence 
  routeSnap :: (Double, Double) -- ^ Origin
            -> Double           -- ^ Scale
            -> Points           -- ^ Points of data
            -> Route
  routeSnap (x0,y0) scl = fmap (cellSnap (x0,y0) scl)
