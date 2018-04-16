def isPrime(x: Int) = {
  import scala.math.sqrt
  x > 1 && (2 to sqrt(x).floor.toInt).forall(x % _ != 0)
}

def primes: Stream[Int] = Stream.from(2).filter(x => isPrime(x))

def gcd(x: Int, y: Int): Int = if (y == 0) { x } else { gcd(y, x % y) }

def isCoprime(x: Int, y: Int) = gcd(x, y) == 1

def totient(x: Int) = (1 to x).filter(isCoprime(_, x)).size

def primeFactors(n: Int) = {
  def loop(n: Int, primes: Stream[Int], acc: List[Int]): List[Int] =
    if (n == 1) acc
    else if (n % primes.head == 0) loop(n / primes.head, primes, primes.head :: acc)
    else loop(n, primes.tail, acc)

  loop(n, primes, Nil).reverse
}

def primeFactorMultiplicity(n: Int) = primeFactors(n).groupBy(x => x).map { case (k, v) => (k, v.size) }
