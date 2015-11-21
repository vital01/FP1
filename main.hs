module MAIN where 

import CSV_PARSER as CSV
import FCM as FCM
import Options.Applicative
import System.Directory
import Control.Exception
import System.IO.Error

data CmdArgs = CmdArgs { input_file :: String, razdel :: Char,
                         first_line, first_column, last_column :: Bool,
                         clusters :: Int, dvtn :: Double,
                         distance_func, init_method, output_file :: String }
                                            
parse_cmd_args :: Parser CmdArgs
parse_cmd_args = CmdArgs
    <$> argument str (metavar "FILE" <> help "Input file")    
    <*> option auto (long "razdel" <> short 'r' <> metavar "CHAR" <> help "Razdelitel in CSV file (defualt ',')" <> value ',')
    <*> option auto (long "first_line"  <> short 'q' <> metavar "BOOL" <> help "Condition to use first line (default true)" <> value True)
    <*> option auto (long "first_column" <> short 'f' <> metavar "BOOL" <> help "Condition to use first column (default true)" <> value True)
    <*> option auto (long "last_column" <> short 'l' <> metavar "BOOL" <> help "Condition to use last column (default false)" <> value False)
    <*> option auto (long "clusters" <> short 'c' <> metavar "INT" <> help "Number of clusters (default 2)" <> value 2)
    <*> option auto (long "dvtn" <> short 'e' <> metavar "DOUBLE" <> help "Tochnost' (default 0.001)" <> value 0.01)
    <*> option auto (long "distance_func" <> short 'd' <> metavar "NAME" <> help "Method of distance cuclulating: \"Haming\" or \"Evclid\"(default \"Evclid\")" <> value "Haming")
    <*> option auto (long "init_method" <> short 'i' <> metavar "NAME" <> help "Method of inirialization: \"Random_matrix\" or \"Random_centers\" (default \"Random_centers\")" <> value "Random_matrix")  
    <*> option str (long "output" <> short 'o' <> metavar "FILE" <> help "Output file" <> value "")


main:: IO()
main = do 
    let args = info (helper <*> parse_cmd_args) fullDesc
    params <- execParser args
    output_matrix params `catch` handler


handler :: IOError -> IO ()
handler ex
    | isDoesNotExistError ex = putStrLn "File not found"
    | otherwise = ioError ex



output_matrix :: CmdArgs -> IO()
output_matrix param 
    | null (output_file param) = do
        cl <- clusterize param
        putStrLn (matrix_to_string cl)
    | otherwise = do
        cl <- clusterize param
        writeFile (output_file param) (show cl)


matrix_to_string :: [[Double]] -> String
matrix_to_string [] =  ""
matrix_to_string m = show (head m) ++ "\n" ++ matrix_to_string (tail m)



clusterize :: CmdArgs -> IO [[Double]]
clusterize param = do
        let fcm = FcmParams { FCM.clusters = (MAIN.clusters param), FCM.dvtn = (MAIN.dvtn param), FCM.distance_func = (MAIN.distance_func param), FCM.init_method = (MAIN.init_method param) }
        parsed <- parse_from_csv param
        cluserized <- fcm_alg parsed fcm
        return cluserized


parse_from_csv :: CmdArgs -> IO [[Double]]
parse_from_csv param = do
    let csvParameters = CsvParams { razd = (MAIN.razdel param), f_line = (MAIN.first_line param), f_column = (MAIN.first_column param), l_column = (MAIN.last_column param) }
    source <- read_file (input_file param)
    csvResult <- parse source csvParameters
    case csvResult of Left errorMessage -> error errorMessage
                      Right objects -> return objects


read_file :: String -> IO String
read_file path = readFile path 