import scala.annotation.tailrec

@tailrec
def isPrime(n: Int, div:Int):Boolean = {
  if (n <= 1) false
  else if (n == 2) true
  else if (div == n) true
  else if (n % div == 0) false
  else isPrime(n, div + 1)
}

def sumPrimes(start: Int, stop: Int): Int = {
  if (start > stop) 0
  else if (isPrime(start, 2)) start + sumPrimes(start + 1, stop)
  else sumPrimes(start + 1, stop)
}

sumPrimes(2, 9)