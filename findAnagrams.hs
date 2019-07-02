module Main where 
import Data.Char  
import Data.List
import System.Environment 

main = do 
 params <- getArgs 
 dictionary <- readFile (head params) 
 --anaWord <- last params 
 --dictList <- lines dictionary 
 putStr "The anagrams of " 
 putStr (last params) 
 putStr " are " 
 print (anagrams(lines dictionary) (last params)) 
 
isAnagram xs xs2 = sort(map toLower (xs)) == sort(map toLower(xs2))

anagrams dictionary word = filter (isAnagram word) dictionary
