module CSV_PARSER where 

import Data.Csv
import Data.Char
import Data.Vector as V
import Data.ByteString.Internal as BSI
import Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 as BSC

data CsvParams = CsvParams { razd :: Char, f_line,f_column,l_column :: Bool}
                                               
parse :: String -> CsvParams -> IO(Either String [[Double]])
parse from_file csvPrms = do
    let options = DecodeOptions { decDelimiter = fromIntegral $ ord $ razd csvPrms }
        csv_data = BSL.pack $ BSI.unpackBytes $ BSC.pack from_file
        parsed = if (f_line csvPrms) then decodeWith options NoHeader csv_data :: Either String (Vector (Vector String)) else decodeWith options HasHeader  csv_data :: Either String (Vector (Vector String))
    case parsed of Left error -> return $ Left error
                   Right parsed_data -> do
                        let checked = remove_columns (l_column csvPrms) (f_column csvPrms) (convert_to_list parsed_data)
                        return $ Right $ checked

remove_columns :: Bool -> Bool -> [[a]] -> [[a]]
remove_columns True True m = remove_heads (remove_lasts m)
remove_columns True False m = remove_heads m
remove_columns False True m = remove_lasts m
remove_columns False False m = m

remove_heads :: [[a]] -> [[a]]
remove_heads [] = []
remove_heads (x:xs) = Prelude.tail x : remove_heads xs

remove_lasts :: [[a]] -> [[a]]
remove_lasts [] = []
remove_lasts (x:xs) = Prelude.init x : remove_lasts xs

convert_to_list :: Vector (Vector String) -> [[Double]]
convert_to_list v 
    | V.null v = []
    | otherwise = Prelude.map read (toList (V.head v)) : convert_to_list (V.tail v)