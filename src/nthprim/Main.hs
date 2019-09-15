import System.Console.ANSI


data Stream a = Stream a (Stream a)

nats :: Stream Integer
nats = natsFrom 0

natsFrom :: Integer -> Stream Integer
natsFrom n = Stream n $ natsFrom $ n + 1

dbl :: Integer -> Stream Integer
dbl n = dbl' n
  where dbl' x = Stream x $ dbl' $ x + n

primes :: Stream Integer
primes = sieve $ natsFrom 2
  where sieve (Stream x xs) = Stream x $ sieve $ diff xs $ dbl x

diff :: Stream Integer -> Stream Integer -> Stream Integer
diff l@(Stream x xs) r@(Stream y ys) =
  case (compare x y) of
    LT -> Stream x $ diff xs r
    EQ -> diff xs ys
    GT -> diff l ys

printRatio :: Integer -> Integer -> IO ()
printRatio v i = do
  setCursorColumn 0
  putStr $ "|" ++ (show v) ++ "/" ++ (show i) ++ "| - (" ++ (show f) ++ "%)"
  where f = floor $ 100 * (realToFrac v) / (realToFrac i)


nth :: Integer -> Stream Integer -> IO Integer
nth v s = do
  hideCursor
  putStr $ "0/" ++ (show v)
  nth' v v s
  where nth' i n (Stream x xs) =
          if n == 0
          then do
            printRatio i i
            putStrLn ""
            showCursor
            return x
          else do
            printRatio (i - n) i
            nth' i (n - 1) xs

nthP n = nth n primes


main :: IO ()
main = do
  putStrLn "Enter position: "
  ns <- getLine
  let n = read ns :: Integer
  p <- nthP n
  putStrLn $ ns ++ "th prime: " ++ (show p)
