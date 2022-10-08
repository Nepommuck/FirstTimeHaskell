-- Mergesort algorithm
mergesort [n] = [n]
mergesort list = merge (mergesort l1) (mergesort l2) 
  where
    (l1, l2) = divide list


-- Dividing a list into halves
divide [] = ([], [])
divide list = (prev2, prev1)
  where 
    prev = divide (tail list)
    prev1 = head list : fst prev
    prev2 = snd prev

-- Merging two sorted lists together
merge [] list = list
merge list [] = list
merge l1 l2 = if h1 < h2
                then h1 : merge (tail l1) l2
              else h2 : merge l1 (tail l2)
  where 
    h1 = head l1
    h2 = head l2


main = do
  let tab = [2, 5, 1, 3, 11, 7, 9, 0, 10, 19, 7, 2];
  putStrLn "Before sorting:";
  print $ tab;

  putStrLn "\nAfter sorting:";
  print $ mergesort tab;
