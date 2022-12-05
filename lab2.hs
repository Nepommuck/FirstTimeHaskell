<<<<<<< HEAD
isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome [c] = True
isPalindrome list = 
    head(list) == last(list) && isPalindrome(middle)
    where middle = tail( init list )


getElemAtIdx :: [a] -> Int -> a
getElemAtIdx list 0 = head(list)
getElemAtIdx list n = getElemAtIdx(tail list) (n-1)


toUpper :: Char -> Char
toUpper c = 
  if('a' <= c && c <= 'z')
    then toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
  else c


capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize str = toUpper(head str) : tail str


pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = 
    [(a,b,c) | a <- [1..n], b <- [a..n], c <- [b..n], a ^ 2 + b ^ 2 == c ^ 2]


numOfPythagoreanTriples :: Int -> Int
numOfPythagoreanTriples n = length(pythagoreanTriples n)


isqrt :: Integral t => t -> t
isqrt = floor . sqrt . fromIntegral


isPrime :: Integral t => t -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = [i | i <- [2..isqrt(n)], n `mod` i == 0] == []


primes :: Int -> [Int]
primes n = eratoSieve [2..n]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve [] = []
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]


-- howManyPrimes :: Integral t => t -> Int
howManyPrimes n = length(primes n)

-- Faster!
countPrimes n =
  checkPrimes n 0
  where
    checkPrimes mx akt | akt > mx = 0
                       | isPrime(akt) = 1 + checkPrimes mx (akt+1)
                       | otherwise = checkPrimes mx (akt+1)


allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (head : tail) = all (== head) tail 


firstFibs n = 
  take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]


prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs) =
  e == x || elem' e xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) =
  2*x : doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs) =
  x*x : squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs)
  | even x    = x : selectEven xs
  | otherwise = selectEven xs


avg :: (Integral a) => [a] -> Double
avg list = 
  (fromIntegral (sum list) ) / (fromIntegral (length list))

geomAvg :: (Integral a) => [a] -> Double
geomAvg list = 
  (fromIntegral (prod' list) ) ** ( 1.0 / (fromIntegral (length' list)))

cubes = [x^3 | x <- [1..10]];

right_triangles(mx) = 
  [(a, b, c) | a <- [1..mx], b <- [a..mx], c <- [b..mx], a^2 + b^2 == c^2]

-- sum' :: (Num a) ==> [a] -> a 
sum' [] = 0
sum' (h:tail) = h + sum' tail


tab = [2, 5, 7]

main = do
    -- print $ isPalindrome "ABCBA"
    -- print $ isPalindrome "GAREK"
    -- print $ getElemAtIdx tab 3
    -- print $ getElemAtIdx tab 0
    -- print $ capitalize "falisz"
    -- print $ numOfPythagoreanTriples 100
    -- print $ primes 100
    -- print $ countPrimes 32000
    -- print $ allEqual [2, 2, 2]
    -- print $ firstFibs 20

    -- print $ prod' tab
    -- print $ length' tab
    -- print $ or' [False, False, False, True, False]
    -- print $ and' [True, True, True, False]
    -- print $ elem' 19 tab
    -- print $ doubleAll tab
    -- print $ squareAll tab
    -- print $ selectEven tab
    print $ avg tab
    print $ geomAvg tab
    print $ 1.0 / 9.0
    print $ 1.0 / fromIntegral (length tab)


    print $ cubes
    print $ right_triangles 50
    print $ sum' [2, 1, 3, 7]
