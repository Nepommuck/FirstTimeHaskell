f1 = (\x y -> x - y) :: Double -> Double -> Double

f2 = (\x y -> sqrt $ (x^2 + y^2)) :: Double -> Double -> Double

f3 = (\x y z -> sqrt $ fromInteger $ (x^2 + y^2 + z^2)) :: Integer -> Integer -> Integer -> Double

twoTimes = (\x -> 2*x)
times2 = (\x -> x*2)
twoToThePower = (\x -> 2^x)
xSquared = (\x -> x^2)
twoDividedBy = (\x -> 2/x)
oneThirdOf = (\x -> x/3)
fourMinus = (\x -> 4-x)

sqrt' = (\x -> x ** 0.5)
abs' = (\x -> max x (-x))

id' = (\x -> x)
const' = (\x y -> x)

f7 = (\x -> x `mod` 2 == 0)
f8 = (\x -> 2 * (sqrt x)^3 * ((sqrt x) + 1))
f9 = (\x -> if x == 2 then 3 else 0)

sumSqr' [] = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f(x) + sumWith f xs

sumSqr = sumWith (^2) 
sumCube = sumWith (^3) 
sumAbs = sumWith abs 

listLength = sumWith (\x -> 1)


prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f [] = 1
prodWith f (x:xs) = f(x) * prodWith f xs

prod = prodWith id

funcFactory n = case n of
 1 -> id
 2 -> \x -> x * x
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5


factorial 0 = 1
factorial n = n * factorial (n-1)

-- expApproxUpTo :: Int -> Double -> Double
-- expApproxUpTo 0 = 0
-- expApproxUpTo n = fromInteger(1) / fromInteger( (factorial n) ) + fromInteger (expApproxUpTo (n-1))


main = do
  print $ funcFactory 5 12
  -- print $ expApproxUpTo 2

  print $ 1.0 / 5

  -- print $ f1 1 2.5
  -- print $ f2 3 4
  -- print $ f3 3 4 (-1)

  -- print $ times2 8
  -- print $ twoToThePower 5
  -- print $ xSquared 5
  -- print $ twoDividedBy 5
  -- print $ oneThirdOf 12
  -- print $ fourMinus 5

  -- print $ sqrt' 64
  -- print $ abs' (-15)
  -- print $ id' 69
  -- print $ const' 1 (1/0)

  -- print $ f7 14
  -- print $ f8 14
  -- print $ f9 14
  -- print $ f9 2

  -- print $ sumSqr' [2, 1, 5]
  -- print $ sumSqr [2, 1, 5]
  -- print $ sumAbs [2, 1, -5]

  -- print $ sumWith (^5) [1..15]
  -- print $ listLength [1..48]

  -- print $ prod [1..5]