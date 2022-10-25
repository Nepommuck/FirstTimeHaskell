isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime 3 = True
isPrime n = 
  if n `mod` 2 == 0 || n `mod` 3 == 0
    then False 
  else checkPrime (n, 5)

checkPrime :: (Int, Int) -> Bool
checkPrime (n, k) = 
  if k^2 > n 
    then True
  else if n `mod` k == 0
    then False
  else checkPrime (n, (k+2))
    

appendPrimes :: ([Int], Int, Int) -> [Int]
appendPrimes (list, akt, mx) = 
  if akt >= mx
    then list
  else if isPrime akt
    then appendPrimes (list++[akt], akt+1, mx)
  else
    appendPrimes (list, akt+1, mx)


getPrimes :: Int -> [Int]
getPrimes n = appendPrimes ([], 2, n)


main = do
  putStrLn "main:";
  print $ getPrimes 200
