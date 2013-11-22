import Lab2
import Data.List

filterDm _ ""  = []
filterDm "" _  = []
filterDm n sn = filter (\(name, surname) -> (name == n) && (surname == sn)) discrete_mathematics

filterP _ ""  = []
filterP "" _  = []
filterP n sn = filter (\(name, surname) -> (name == n) && (surname == sn)) programming

main = do
putStrLn "DM to Prog List:"
mapM_ putStrLn $ map (\ (pos, (n, sn)) -> show pos ++ "). " ++ n ++ " " ++ sn) 
	$ zip [1..] $ (students >>= (\st -> filterDm (name st) (surname st))) >>=
		(\(n, sn) -> if(null $ filterP n sn) then [(n, sn)] else [])