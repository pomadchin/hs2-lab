import Lab2
import Data.List

main = do
putStrLn "Для всех студентов, посещающих дискретную математику: "
putStrLn "если он записан на программирование, сделать так, чтобы на программирование он записан не был, "
putStrLn "и наоборот (если не записан, то записать): "
mapM_ putStrLn $ map (\ (pos, (n, sn)) -> show pos ++ "). " ++ n ++ " " ++ sn) 
	$ zip [1..] $ discrete_mathematics >>=
		(\(n, sn) -> if(null $ filter (\(name, surname) -> (name == n) && (surname == sn)) programming) then [(n, sn)] else [])