--Sorting and Permutaitons in Haskell
osort :: Ord a => Char -> [a] ->[a]
osort 'f' xs= quicksort(xs)
osort 'b' xs = reverse (quicksort(xs))
osort x xs= [] 

permute :: Ord a => [Int]->[a] ->[a]
permute xs [] = []
permute [] xs2 = []
permute xs xs2 = [quicksort(xs2) !! x| x <- map(subtract 1) xs, (maximum(xs) <= length(xs2))]



quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 
    
-- http://learnyouahaskell.com/recursion#quick-sort
  
data Choose = Sort Char | Permute [Int] 

reorder :: Ord a => Choose -> [a] -> [a]
reorder (Sort 'f') a = osort 'f' a
reorder (Sort 'b') a = osort 'b' a
reorder (Sort c) a = []
reorder (Permute b) a= permute b a
