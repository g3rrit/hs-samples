type Key = (Integer, Integer) 

keygen :: Integer -> Integer ->Integer -> (Key, Key)
keygen p q c = ((e, n), (d, n))
  where n = p * q
        phi = (p - 1) *  (q - 1)
        nf = notFactors phi
        e = nf !! (fromInteger $ c `mod` (fromIntegral $ length nf))
        (_, d, _) = extGCD phi e

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (1, 0, a)
extGCD a b = (t, s - q * t, abs g)
  where (q, r) = a `quotRem` b
        (s, t, g) = extGCD b r

notFactors :: Integer -> [Integer]
notFactors n = nf n []
  where nf 1 r = r
        nf c r = if (n `mod` c) == 0 
                 then nf (c - 1) r
                 else nf (c - 1) $ c : r

factors :: Integer -> [Integer]
factors n =
  case fs of
    [] -> [n]
    _  -> fs ++ factors (n `div` (head fs))
  where fs = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

crypt :: Integer -> Key -> Integer
crypt m (k, n) = (m ^ k) `mod` n
