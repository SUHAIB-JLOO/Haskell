-- Aufgabe 1 --- Gruppe King Salat -----

-- Eigene Listenfunktionen --
myhead :: [Int] -> Int
myhead (x:xs) = x


mytail :: [Int] -> [Int]
mytail (x:xs) = xs



mylast :: [a] -> a
mylast [x] = x
mylast (x:xs) = mylast xs


myinit :: [a] -> [a]
myinit [x] = []
myinit (x:xs) = x : myinit xs


myreverse :: [Int] -> [Int]
myreverse [] = []
myreverse x = mylast x : myreverse (myinit x)

-- FizzBuzz --
toStr :: (Integral a, Show a) => a -> String
toStr n
  | mod n 15 == 0 = "FizzBuzz"
  | mod n 3  == 0 = "Fizz"
  | mod n 5  == 0 = "Buzz"
  | otherwise     = show n
  
fizzBuzz :: (Integral a, Show a) => a -> [String]
fizzBuzz x = [toStr n | n<- [1..x]]



-- Fibonacci sequence --
fibonacci :: Num a => [a]
fibonacci = [0,1] ++ fibonacciSeq 0 1

fibonacciSeq :: Num a => a -> a -> [a]
fibonacciSeq x y = [x+y] ++ fibonacciSeq y (x+y)

-- Primes --
primes :: [Int]
primes = [n | n <- [1..] ,isPrime n]

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = all (\x->mod n x /= 0) [2..(div n 2)]

-- Prime Factor Decomposition --
primeFactors :: Int -> [Int]
primeFactors n
  | n == 1 = []
  | isPrime n = [n]
  | otherwise = fac : primeFactors (div n fac)
  where
    fac = head (filter (\x-> x>1 && mod n x == 0) primes)

