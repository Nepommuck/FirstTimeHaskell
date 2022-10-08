-- Logical negation
neg :: Bool -> Bool
neg True = False
neg False = True

-- Logical or
lub :: Bool -> Bool -> Bool
lub p q
  | p = True
  | q = True
  | otherwise = False

-- Absolute value
absol x = max x (-x)

-- Factorial function
silnia :: Integer -> Integer
silnia 0 = 1
silnia n = n * silnia (n-1)

-- Integral power
pow :: Double -> Int -> Double
pow a 0 = 1
pow a n =
  if n > 0
    then a * pow a (n-1)
    else 1 / a * pow a (n+1)


main = do
  print $ neg False
  print $ False `lub` True
  print $ absol (-6)
  print $ silnia 5
  print $ silnia 11
  print $ pow 2 10
  print $ pow 10 (-1)
