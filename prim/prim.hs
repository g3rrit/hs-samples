

primes = sieve [2..]

sieve (p:ps) = p : sieve [x | x <- ps, mod x p /= 0]

twin (x,y) = y == x + 2

twin_primes = filter twin(zip primes(tail primes))