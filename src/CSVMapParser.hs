module CSVMapParser where
    
  import Data.List (foldl',transpose)
  import qualified Data.ByteString.Lazy.Char8 as BStr
  import qualified Data.Map as Map
  import Data.Map ((!))
  
  
  -- Read In a row major formatted dataset with names in the first row.
  readCSV :: Char
          -> FilePath 
          -> IO (Map.Map String [BStr.ByteString])
  readCSV delimiter name = do
      sheet <- (transpose . map (BStr.split delimiter)) . 
                BStr.lines <$> BStr.readFile name
      return $ foldl' go Map.empty sheet
    where go m (x:xs) = Map.insert (BStr.unpack x) xs m


  unpackData :: Map.Map String [BStr.ByteString] -> String -> [Double]
  unpackData dat serie = map (read . BStr.unpack) $ dat ! serie