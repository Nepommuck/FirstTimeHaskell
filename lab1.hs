-- Lab 1
add x y = x + y

dzielCalk a b =  if a < b  
                    then 0
                  else 1 + dzielCalk (a-b) b 


sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt (x^2 + y^2)


-- Zadania
-- 1.
vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt(x^2 + y^2 + z^2)

-- 2.
swap :: (Int, Char) -> (Char, Int)
swap (i, c) = (c, i)

-- 3.
threeEqual :: (Int, Int, Int) -> Bool
threeEqual (a, b, c) = a == b && a == c

-- 4.
triangleArea :: (Double, Double, Double) -> Double
triangleArea (a, b, c) = sqrt ((sum [a, b, c] - 2*a) * (sum [a, b, c] - 2*b) * (sum [a, b, c] - 2*c) * sum [a, b, c] / 16)

toUpper :: Char -> Char
toUpper c = 
  if('a' <= c && c <= 'z')
    then toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
  else c

toLower :: Char -> Char
toLower c = 
  if('A' <= c && c <= 'Z')
    then toEnum (fromEnum c - fromEnum 'A' + fromEnum 'a')
  else c

isDigit :: Char -> Bool
isDigit c = ('0' <= c && c <= '9')

charToNum :: Char -> Int
charToNum c =
  if( isDigit(c) )
    then fromEnum(c) - fromEnum('0')
  else -1

main = do
  let tab = [2, 5, 1, 3, 11, 7, 9, 0, 10, 19, 7, 2];
  putStrLn "main:";

  print $ vec3DLen (4, -3, 0);
  print $ swap (14, 'b');
  print $ threeEqual (1, 2, 1);

  print $ triangleArea (3, 4, 5);

  print $ charToNum '8'
  print $ charToNum '1'
  print $ charToNum '3'
  print $ charToNum '1'
  print $ charToNum 'r'

