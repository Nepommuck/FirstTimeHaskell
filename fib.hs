-- Fibonacci sequence
secLast lis = last (init lis)

fib :: Int -> [Integer]
fib 1 = [1]
fib 2 = [1, 1]
fib n = prev ++ [last prev + secLast prev]
  where
    prev = fib (n-1)

main = print $ fib 50