-- Recaman's Sequence 
-- By starting with RecaSeq it becomes easier to write the other two methods 

recaSeq :: Int -> [Int]

--Case for zero
recaSeq 0 = [0]
recaSeq xs =

--Declarations allow for easier typing and neater code, may want to go over RS theory and name choices

  let list = recaSeq (xs - 1)
      next= last list
      lastList= (last (list))-xs
  in
    if lastList >= 0 && (lastList `notElem` list) then
      list ++ [lastList]
    else
      list ++ [next + xs]

--Just take the last element from recaSeq

recaMan :: Int -> Int
recaMan xs= last (recaSeq(xs))

--Map is used to apply functions to lists!

recaList :: [Int] -> [Int]
recaList xs = map (recaMan) xs
