cubes = [x^3 | x <- [1..10]];

right_triangles(mx) = 
  [(a, b, c) | a <- [1..mx], b <- [a..mx], c <- [b..mx], a^2 + b^2 == c^2]

-- sum' :: (Num a) ==> [a] -> a 
sum' [] = 0
sum' (h:tail) = h + sum' tail

fiveToPower

main = do
  print $ cubes
  print $ right_triangles 50
  print $ sum' [2, 1, 3, 7]