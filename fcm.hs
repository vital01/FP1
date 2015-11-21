module FCM where 

import Data.List.Split
import System.Random


data FcmParams = FcmParams { clusters :: Int, dvtn :: Double, distance_func :: String, init_method :: String }


fcm_alg :: [[Double]] -> FcmParams -> IO [[Double]]
fcm_alg obj par = do
    matrix <- generate_first_matrix par obj
    return $ fcm_alg_cycle obj matrix (dvtn par) (choose_distance_func par)


generate_first_matrix :: FcmParams -> [[Double]] -> IO[[Double]]
generate_first_matrix par o 
    | ((init_method par) == "Random_matrix") = generate_random_matrix (length o) (clusters par)
    | otherwise = return $ calc_new_matrix o (generate_random_centers (clusters par) o) (choose_distance_func par)


choose_distance_func :: FcmParams -> ([Double] -> [Double] -> Double)
choose_distance_func par
    | ((distance_func par) == "Haming") = haming_distance
    | otherwise = evklid_distance
    


fcm_alg_cycle :: [[Double]] -> [[Double]] -> Double -> ([Double] -> [Double] -> Double) -> [[Double]]
fcm_alg_cycle o m e f
    | (maximum_matrix_diff m (calc_new_matrix o (center_matrix o m) f)) < e = calc_new_matrix o (center_matrix o m) f
    | otherwise = fcm_alg_cycle o (calc_new_matrix o (center_matrix o m) f) e f




center_matrix :: [[Double]] -> [[Double]] -> [[Double]]
center_matrix _ y 
    |null (head y) = []
center_matrix x y =  center_string x (get_heads y) : center_matrix x (remove_heads y)

center_string :: [[Double]] -> [Double] -> [Double]
center_string x _ 
    | null (head x) = []
center_string x y = ((center_elem (get_heads x) y)/(summ_of_sqr (y))) : center_string (remove_heads x) y 

center_elem :: [Double] -> [Double] -> Double
center_elem [] [] = 0
center_elem (x:xs) (y:ys) = (x*(y^2)) + center_elem xs ys

get_heads :: [[a]] -> [a]
get_heads [] = []
get_heads (x:xs) = head x : get_heads xs

remove_heads :: [[a]]  -> [[a]]
remove_heads [] = []
remove_heads (x:xs) = tail x : remove_heads xs

summ_of_sqr :: [Double] -> Double
summ_of_sqr [] = 0
summ_of_sqr (x:xs) = x^2 + summ_of_sqr xs

haming_distance :: [Double] -> [Double] -> Double
haming_distance [] _ = 0
haming_distance _ [] = 0
haming_distance (x:xs) (y:ys) = (abs (x-y)) + haming_distance xs ys

evklid_distance :: [Double] -> [Double] -> Double
evklid_distance x y = evklid_distance_with_acc x y 0

evklid_distance_with_acc :: [Double] -> [Double] -> Double -> Double
evklid_distance_with_acc [] _ acc = sqrt acc
evklid_distance_with_acc _ [] acc = sqrt acc
evklid_distance_with_acc (x:xs) (y:ys) acc = evklid_distance_with_acc xs ys (acc + (x-y)^2)

maximum_matrix_diff :: [[Double]] -> [[Double]] -> Double
maximum_matrix_diff x y = maximum (concat (matrix_diff x y))

matrix_diff :: [[Double]] -> [[Double]] -> [[Double]]
matrix_diff _ [] = []
matrix_diff [] _ = []
matrix_diff (x:xs) (y:ys) = (matrix_string_diff x y):(matrix_diff xs ys)

matrix_string_diff :: [Double] -> [Double] -> [Double]
matrix_string_diff _ [] = []
matrix_string_diff [] _ = []
matrix_string_diff (x:xs) (y:ys) = (abs (x-y)) : matrix_string_diff xs ys


--					objects			Vl			distance func						mu
calc_new_matrix :: [[Double]] -> [[Double]] -> ([Double] -> [Double] -> Double) -> [[Double]]
calc_new_matrix [] _ _ = []
calc_new_matrix (x:xs) y z = (calc_new_matrix_string x y y z) : calc_new_matrix xs y z

--						 objects(i)		Vl			Vl				distance func					mu(i)
calc_new_matrix_string :: [Double] -> [[Double]] -> [[Double]]-> ([Double] -> [Double]-> Double) -> [Double]
calc_new_matrix_string _ [] _ _ = []
calc_new_matrix_string x (y:ys) z t = (replace_NaN (calc_new_matrix_element x y z 0 t)) : calc_new_matrix_string x ys z t

--						   objects(i)	Vl(i)		Vl			accum			distance func				mu(i,j)
calc_new_matrix_element :: [Double] -> [Double] -> [[Double]] -> Double -> ([Double] -> [Double]-> Double) -> Double
calc_new_matrix_element _ _ [] acc _ = acc**(-1)
calc_new_matrix_element x y (z:zs) acc f = calc_new_matrix_element x y zs (acc + ((f x y)/(f x z))^2) f

replace_NaN :: Double -> Double
replace_NaN value
    | isNaN value = 1
    | otherwise = value

generate_random_centers :: Int -> [[Double]] -> [[Double]]
generate_random_centers count o = take count o


generate_random_matrix :: Int -> Int -> IO [[Double]]
generate_random_matrix co cl = do
    rand <- generate_random_string co cl
    return $ normalize_matrix (chunksOf cl rand)


normalize_matrix :: [[Double]] -> [[Double]]
normalize_matrix a = map (\ row -> map (/ (sum row)) row) a


generate_random_string :: Int -> Int -> IO [Double]
generate_random_string co cl = do
    stdGen <- newStdGen
    return $ take (co * cl) (randoms stdGen :: [Double])